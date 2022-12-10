{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module HDL.Interpreter () where

import           Control.Monad.Except (ExceptT, MonadError(throwError)
                                     , MonadIO(liftIO), runExceptT, liftEither)
import           Control.Monad.State (MonadState(get), StateT, runStateT)
import           Data.Bits (Bits(shiftL, shiftR, (.|.)), complement, (.&.))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           HDL.Parser (Chip(..), ChipImpl(..), ConnSide(..), Pin(..)
                           , connSideName, pinName, Part(partName, partConns)
                           , Conn(Conn, connValue, connTarget), connTargetName
                           , connValueName)
import           Prelude hiding (lookup)
import           Text.Printf (printf)
import           PyF (fmt)
import           Data.Int (Int16)
import           Data.Maybe (fromMaybe)
import           Lens.Micro.Platform (makeLenses, use, (%=), view, _2, (^.), (&)
                                    , ix, at, (.~), (?~), mapped, (^?), preview
                                    , (.=), (%~), set)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.Monad (forM_)
import           Data.Functor ((<&>))
import           Control.Applicative (Applicative(liftA2))
import           GHC.Base (returnIO)

type Name = Text

data Value = Bit Int16
           | Bus Int Int16

instance Show Value where
  show (Bit v) = printf "Bit %b" (0x1 .&. v)
  show (Bus n v) = printf "Bus %d %0*b (%d)" n n v v

instance Eq Value where
  (==) (Bit _) (Bit _) = True
  (==) (Bus x _) (Bus y _) = x == y
  (==) _ _ = False

type PinTable = Map Name Value

data SymbolTable =
  SymbolTable { _chipTable :: IO (Map Name (Chip, Vector PinTable))
              , _pinTable :: PinTable
              , _seqChipCount :: Int
              }

$(makeLenses ''SymbolTable)

type InterpreterEnv = ExceptT String (StateT SymbolTable IO)

note :: (MonadError String m) => String -> Maybe a -> m a
note e Nothing = throwError e
note _ (Just a) = pure a

lookupPinTab :: (MonadError String m) => Name -> PinTable -> m Value
lookupPinTab "true" _ = pure $ Bit 1
lookupPinTab "false" _ = pure $ Bit 0
lookupPinTab name pinTab =
  note [fmt|Pin '{name}' not found|] $ M.lookup name pinTab

nBitOne :: Int -> Int16
nBitOne n = iterate ((.|. 1) . (`shiftL` 1)) 1 !! n

evalConnSide :: (MonadError String m) => ConnSide -> PinTable -> m Value
evalConnSide side pinTab = do
  v <- lookupPinTab (connSideName side) pinTab
  case (side, v) of
    (Id _, _) -> pure v
    (Index _ idx, Bus bc v')
      -> if idx < 0 || idx >= bc
         then throwError [fmt|Index '{idx}' is out of bound|]
         else pure $ Bit ((v' `shiftR` idx) .&. 1)
    (Range _ a b, Bus bc v')
      -> if a < 0 || b <= a || b > bc
         then throwError [fmt|Range '{a}..{b}' is out of bound|]
         else pure $ Bus (b - a + 1) (v' `shiftR` a .&. nBitOne (b - a))
    _ -> throwError "Invalid operation"

chipPinsToPinTable :: [Pin] -> PinTable -> PinTable
chipPinsToPinTable pins pinTab = foldr
  (\p -> M.insert
     (pinName p)
     (case p of
        PinSingle _   -> Bit 0
        PinMulti _ bc -> Bus bc 0))
  pinTab
  pins

tryAdd :: Int -> Vector PinTable -> Vector PinTable
tryAdd n vec = vec V.++ V.replicate (n - V.length vec) M.empty

-- Return symbol table of the next cycle
evalImpl :: Chip -> ChipImpl -> InterpreterEnv (Maybe PinTable)
evalImpl _ (BuiltIn "Nand") = do
  pinTab <- use pinTable
  a <- lookupPinTab "a" pinTab
  b <- lookupPinTab "b" pinTab
  out <- case (a, b) of
    (Bit x, Bit y) -> pure . Bit $ complement (x .&. y)
    _ -> throwError "Invalid operation"
  pinTable %= M.insert "out" out
  return Nothing
evalImpl _ (BuiltIn "DFF") = do
  symTable <- get
  seqChipCount %= (+ 1)
  let seqCount = symTable ^. seqChipCount
  --
  maybeDFF <- liftIO $ symTable ^. chipTable <&> view (at "DFF")
  dff@(_, vecPinTab) <- note "Chip 'DFF' not found" maybeDFF
  --
  let vec = tryAdd seqCount vecPinTab
  -- Get the stored input
  let currentOut =
        fromMaybe (Bit 0) $ view (at "out") $ vec ^. ix (seqCount - 1)
  pinTable %= (at "out" ?~ currentOut)
  -- Save the next output
  clockedIn <- lookupPinTab "in" (symTable ^. pinTable)
  let outPinTab = vec & ix (seqCount - 1) . at "out" ?~ clockedIn
  chipTable %= fmap (at "DFF" ?~ (dff & _2 .~ outPinTab))
  return $ Just (outPinTab ^. ix (seqCount - 1))
  -- where
     -- Store new value 
     -- in' <- liftEither $ lookupPinTab "in" (pinTable symTab)
     -- chip <- liftIO
     --   (note [fmt|Chip 'DFF' not found|] . M.lookup "DFF" <$> chipTable symTab)
     --   >>= liftEither
     -- return $ Just chip { chipIns = [valToPin "in" in'] }
     -- valToPin name = \case
     --   Bit v    -> PinSingle name v
     --   Bus bc v -> PinMulti name bc v
evalImpl _ (BuiltIn _) = undefined
evalImpl chip (Parts parts) = do
  forM_ parts
    $ \part -> do
      symTab <- get
      let cname = partName part
      maybeChip <- liftIO $ (symTab ^. chipTable) <&> view (at cname)
      partChip <- fst <$> note [fmt|Chip '{cname}' not found|] maybeChip
      maybePrevPinTab <- liftIO
        $ (fmap . fmap) snd (symTab ^. chipTable) <&> view (at $ chipName chip)
      prevPinTab <- note "Unexpected error" maybePrevPinTab
        <&> preview (ix $ symTab ^. seqChipCount)
      chipLocalSymTab <- supplyInputs partChip (partConns part)
        $ M.union (symTab ^. pinTable) (fromMaybe M.empty prevPinTab)
      (chipOutNextVal, chipOutSymTab) <- liftIO
        $ runStateT
          (runExceptT $ evalImpl partChip (chipImpl partChip))
          (symTab
           & pinTable .~ chipPinsToPinTable (chipOuts partChip) chipLocalSymTab)
      -- Update the state of the container chip
      updatedPinTab <- setOutputs
        partChip
        (partConns part)
        (symTab ^. pinTable)
        (chipOutSymTab ^. pinTable)
      pinTable .= updatedPinTab
      -- Save the next state
      chipOutNextVal <- liftEither chipOutNextVal
      case chipOutNextVal of
        Nothing -> pure ()
        Just m  -> do
          clockedPinTab <- setOutputs partChip (partConns part) M.empty m
          let seqCount = symTab ^. seqChipCount
          -- w <- liftIO $ symTab ^. chipTable
          -- let q = w ^. at (chipName chip) <&> _2 %~ (set (ix $ seqCount - 1) clockedPinTab . tryAdd seqCount)
          chipTable
            %= fmap
              (at (chipName chip)
               %~ fmap
                 (_2
                  %~ (set (ix $ seqCount - 1) clockedPinTab . tryAdd seqCount)))
  return Nothing
  where
    --       -- Eval the nested chip
    --       (chipOutEither, chipOutSymTab) <- liftIO
    --         $ runStateT
    --           (runExceptT $ evalImpl internalChip (chipImpl internalChip))
    --           symTab { pinTable =
    --                      chipPinsToPinTable (chipOuts internalChip) chipLocalSymTab
    --                  }
    --       -- Update the state of the container chip
    --       updatedPinTab <- liftEither
    --         $ setOutputs
    --           internalChip
    --           (partConns part)
    --           (pinTable symTab)
    --           (pinTable chipOutSymTab)
    --       -- _ <- trace (show updatedPinTab) $ pure ()
    --       put $ symTab { pinTable = updatedPinTab }
    --       -- Update the array of chips if necessary
    --       chipOut <- liftEither chipOutEither
    --       case (oldChip, chipOut) of
    --         (Nothing, Just c) -> go (chip) ps (seqCount + 1)
    --         (Just _, Just c) -> go (chip) ps (seqCount + 1)
    --         _ -> go chip ps seqCount
    --       -- if isNothing oldChip and then
    --       --   go (chip { chipChildren = (chipChildren chip) <> [] })
    --       -- return Nothing
    --       -- chipOutSymTab <- liftIO
    --       --   $ execStateT
    --       --     (runExceptT $ evalImpl (chipImpl chip) Nothing)
    --       --     symTab { pinTable =
    --       --                chipPinsToPinTable (chipOuts chip) chipLocalSymTab
    --       --            }
    --       -- _ <- trace
    --       --   ("Run "
    --       --    <> unpack (chipName chip)
    --       --    <> " done. "
    --       --    <> show (pinTable chipOutSymTab)
    --       --    <> show (pinTable symTab))
    --       --   $ pure ()
    --       -- go ps seqCount
    setOutputs :: (MonadError String m)
               => Chip
               -> [Conn]
               -> PinTable
               -> PinTable
               -> m PinTable
    setOutputs chip conns parentSymTab chipSymTab =
      setPins (swapSide <$> outConns) chipSymTab =<< initOutPins
      where
        outConns =
          filter ((`elem` (pinName <$> chipOuts chip)) . connTargetName) conns

        -- Input and output pins that supports indexing are already in the table
        -- Internal pin does not support indexing
        connToPin conn = case ( connTarget conn
                              , connValue conn
                              , M.lookup (connTargetName conn) chipSymTab) of
          (Id t, _, _)
            | t
              `elem` ["true", "false"] -> throwError "Invalid output pin name"
          (Id _, Id _, Just v) -> pure v
          (Index _ _, Id _, _) -> pure $ Bit 0
          (Range _ a b, Id _, _) -> pure $ Bus (b - a + 1) 0
          (_, _, Just v) -> pure v -- to be discard
          _ -> throwError "Invalid operation"

        initOutPins = foldr
          (\conn -> liftA2 (M.insertWith (const id) (connValueName conn))
           $ connToPin conn)
          (pure parentSymTab)
          outConns

        swapSide (Conn target value) = Conn value target

    supplyInputs
      :: (MonadError String m) => Chip -> [Conn] -> PinTable -> m PinTable
    supplyInputs chip conns parentSymTab = setPins inConns parentSymTab
      $ chipPinsToPinTable (chipIns chip) M.empty
      where
        inConns =
          filter ((`elem` (pinName <$> chipIns chip)) . connTargetName) conns

    -- Set pins for local symbol table using data from parent symbol table
    setPins
      :: (MonadError String m) => [Conn] -> PinTable -> PinTable -> m PinTable
    setPins conns' parentSymTab symTab =
      foldr ((=<<) . updateInVal) (pure symTab) conns'
      where
        modifyBits num pos bitCount val =
          (num .&. complement (nBitOne bitCount `shiftL` pos))
          .|. (val `shiftL` pos)

        updateInVal conn localSymTab = do
          -- _ <- trace
          --   ("p = "
          --    <> show parentSymTab
          --    <> "\nc = "
          --    <> show localSymTab
          --    <> "\n")
          --   (pure M.empty)
          -- _ <- trace ("Target = " <> show (connTarget conn)) (pure M.empty)
          target <- lookupPinTab (connTargetName conn) localSymTab
          -- _ <- trace ("\t = " <> show target) (pure M.empty)
          -- _ <- trace ("Val = " <> show (connValue conn)) (pure M.empty)
          val <- evalConnSide (connValue conn) parentSymTab
          -- _ <- trace ("\t = " <> show val) (pure M.empty)
          -- _ <- trace "Done" (pure M.empty)
          let connRHSName = connValueName conn
          rhs <- case (connTarget conn, target, val) of
            (Id _, x, y) -- a=b
              | x == y -> pure y
              | otherwise -> throwError
                [fmt|'{show x}' and '{show y}' is not compatible|]
            (Index _ idx, Bus bc bsv, Bit btv) ->
              -- a[1]=b
              pure $ Bus bc (modifyBits bsv idx 1 btv)
            (Range _ a b, Bus bc bsv, Bus bc' bsv')
              | bc' /= a - b -> throwError
                [fmt|Range '{a}..{b}' is larger than the bit count of value|]
              | otherwise -> pure $ Bus bc (modifyBits bsv a bc' bsv')
            (Range _ a b, Bus bc bsv, Bit _)
              | connRHSName `elem` ["true", "false"] -> pure
                $ Bus
                  bc
                  (modifyBits
                     bsv
                     a
                     (b - a + 1)
                     (if connRHSName == "true"
                      then nBitOne $ b - a + 1
                      else 0))
            _ -> throwError "Invalid operation"
          return $ M.insert (connTargetName conn) rhs localSymTab
-- eval :: Chip -> PinTable -> IO (Maybe Chip)
-- eval chip pinTab = do
--   (s, v) <- runStateT
--     (runExceptT $ evalImpl chip (chipImpl chip))
--     SymbolTable { chipTable = loadAllChips
--                 , pinTable = chipPinsToPinTable (chipOuts chip) pinTab
--                 }
--   case s of
--     Left err -> print err >> return Nothing
--     Right c  -> print
--       (filter
--          ((`elem` (pinName <$> chipIns chip <> chipOuts chip)) . fst)
--          (M.toList $ pinTable v))
--       >> return c
-- testChip :: IO ()
-- testChip = do
--   chip <- loadChip "ALU"
--   eval
--     chip
--     (M.fromList
--        [ ("x", Bus 16 5)
--        , ("y", Bus 16 10)
--        , ("zx", Bit 1)
--        , ("nx", Bit 1)
--        , ("zy", Bit 1)
--        , ("ny", Bit 1)
--        , ("f", Bit 1)
--        , ("no", Bit 0)])