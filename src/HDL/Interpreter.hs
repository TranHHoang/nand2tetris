{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BinaryLiterals #-}

module HDL.Interpreter (eval, testChip) where

import           Control.Monad (forM_)
import           Control.Monad.Except (ExceptT, MonadError(throwError)
                                     , MonadIO(liftIO), liftEither, runExceptT)
import           Control.Monad.State (MonadState(get, put), StateT, execStateT
                                    , runStateT)
import           Data.Bits (Bits(shiftL, shiftR, (.|.)), complement, (.&.))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Word (Word16)
import           HDL.Parser (Chip(..), ChipImpl(..), Conn(..), ConnSide(..)
                           , Part(..), Pin(..), connSideName, connTargetName
                           , connValueName, pinName, loadAllChips, loadChip)
import           Prelude hiding (lookup)
import           Text.Printf (printf)
import           PyF (fmt)
import           Control.Applicative (Applicative(liftA2))

type Name = Text

data Value = Bit Word16
           | Bus Int Word16

instance Show Value where
  show (Bit v) = printf "Bit %b" (0x1 .&. v)
  show (Bus n v) = printf "Bus %d %0*b (%d)" n n v v

instance Eq Value where
  (==) (Bit _) (Bit _) = True
  (==) (Bus x _) (Bus y _) = x == y
  (==) _ _ = False

type PinTable = Map Name Value

data SymbolTable =
  SymbolTable { chipTable :: IO (Map Name Chip), pinTable :: PinTable }

type InterpreterEnv = ExceptT String (StateT SymbolTable IO)

note :: String -> Maybe a -> Either String a
note e Nothing = Left e
note _ (Just a) = Right a

lookupPinTab :: Name -> PinTable -> Either String Value
lookupPinTab "true" _ = Right $ Bit 1
lookupPinTab "false" _ = Right $ Bit 0
lookupPinTab name pinTab =
  note [fmt|Pin '{name}' not found|] $ M.lookup name pinTab

nBitOne :: Int -> Word16
nBitOne n = iterate ((.|. 1) . (`shiftL` 1)) 1 !! n

evalConnSide :: ConnSide -> PinTable -> Either String Value
evalConnSide side symTab = do
  v <- lookupPinTab (connSideName side) symTab
  case (side, v) of
    (Id _, _) -> Right v
    (Index _ idx, Bus bc v') -> if idx < 0 || idx >= bc
                                then Left [fmt|Index '{idx}' is out of bound|]
                                else Right $ Bit ((v' `shiftR` idx) .&. 1)
    (Range _ a b, Bus bc v')
      -> if a < 0 || b <= a || b > bc
         then Left [fmt|Range '{a}..{b}' is out of bound|]
         else Right $ Bus (b - a + 1) (v' `shiftR` a .&. nBitOne (b - a))
    _ -> Left "Invalid operation"

chipPinsToPinTable :: [Pin] -> PinTable -> PinTable
chipPinsToPinTable pins pinTab = foldr
  (\p -> M.insert
     (pinName p)
     (case p of
        PinSingle _   -> Bit 0
        PinMulti _ bc -> Bus bc 0))
  pinTab
  pins

evalImpl :: ChipImpl -> InterpreterEnv ()
evalImpl (BuiltIn "Nand") = do
  symTab <- get
  case (do
          a <- lookupPinTab "a" (pinTable symTab)
          b <- lookupPinTab "b" (pinTable symTab)
          case (a, b) of
            (Bit x, Bit y) -> Right . Bit $ complement (x .&. y)
            _ -> Left "Error") of
    Left err  -> throwError err
    Right out -> put
      $ symTab { pinTable = M.insert "out" out (pinTable symTab) }
evalImpl (BuiltIn _) = undefined
evalImpl (Parts parts) = do
  forM_ parts
    $ \part -> do
      symTab <- get
      let cname = partName part
      ceither <- liftIO
        $ note [fmt|Chip '{cname}' not found|] . M.lookup cname
        <$> chipTable symTab
      chip <- liftEither ceither
      chipLocalSymTab <- liftEither
        $ supplyInputs chip (partConns part) (pinTable symTab)
      -- Stub outputs
      -- let chipLocalSymTab' = foldl
      --       (\m p -> M.insert
      --          (pinName p)
      --          (case p of
      --             PinSingle _  -> Bit 0
      --             PinMulti _ n -> Bus n 0)
      --          m)
      --       chipLocalSymTab
      --       (chipOuts chip)
      chipOutSymTab <- liftIO
        $ execStateT
          (runExceptT $ evalImpl (chipImpl chip))
          symTab { pinTable =
                     chipPinsToPinTable (chipOuts chip) chipLocalSymTab
                 }
      -- _ <- trace
      --   ("Run "
      --    <> unpack (chipName chip)
      --    <> " done. "
      --    <> show (pinTable chipOutSymTab)
      --    <> show (pinTable symTab))
      --   $ pure ()
      updatedPinTab <- liftEither
        $ setOutputs
          chip
          (partConns part)
          (pinTable symTab)
          (pinTable chipOutSymTab)
      -- _ <- trace (show updatedPinTab) $ pure ()
      put $ symTab { pinTable = updatedPinTab }
  where
    setOutputs
      :: Chip -> [Conn] -> PinTable -> PinTable -> Either String PinTable
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
            | t `elem` ["true", "false"] -> Left "Invalid output pin name"
          (Id _, Id _, Just v) -> Right v
          (Index _ _, Id _, _) -> Right $ Bit 0
          (Range _ a b, Id _, _) -> Right $ Bus (b - a + 1) 0
          (_, _, Just v) -> Right v -- to be discard
          _ -> Left "Invalid operation"

        initOutPins = foldr
          (\conn -> liftA2 (M.insertWith (const id) (connValueName conn))
           $ connToPin conn)
          (pure parentSymTab)
          outConns

        swapSide (Conn target value) = Conn value target

    supplyInputs :: Chip -> [Conn] -> PinTable -> Either String PinTable
    supplyInputs chip conns parentSymTab = setPins inConns parentSymTab
      $ chipPinsToPinTable (chipIns chip) M.empty
      where
        inConns =
          filter ((`elem` (pinName <$> chipIns chip)) . connTargetName) conns

    -- Set pins for local symbol table using data from parent symbol table
    setPins :: [Conn] -> PinTable -> PinTable -> Either String PinTable
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
              | x == y -> Right y
              | otherwise
                -> Left [fmt|'{show x}' and '{show y}' is not compatible|]
            (Index _ idx, Bus bc bsv, Bit btv) ->
              -- a[1]=b
              Right $ Bus bc (modifyBits bsv idx 1 btv)
            (Range _ a b, Bus bc bsv, Bus bc' bsv')
              | bc' /= a - b -> Left
                [fmt|Range '{a}..{b}' is larger than the bit count of value|]
              | otherwise -> Right $ Bus bc (modifyBits bsv a bc' bsv')
            (Range _ a b, Bus bc bsv, Bit _)
              | connRHSName `elem` ["true", "false"] -> Right
                $ Bus
                  bc
                  (modifyBits
                     bsv
                     a
                     (b - a + 1)
                     (if connRHSName == "true"
                      then nBitOne $ b - a + 1
                      else 0))
            _ -> Left "Invalid operation"
          return $ M.insert (connTargetName conn) rhs localSymTab

eval :: Chip -> PinTable -> IO ()
eval chip pinTab = do
  (s, v) <- runStateT
    (runExceptT $ evalImpl (chipImpl chip))
    SymbolTable { chipTable = loadAllChips
                , pinTable = chipPinsToPinTable (chipOuts chip) pinTab
                }
  case s of
    Left err -> print err
    Right _  -> print
      $ filter
        ((`elem` (pinName <$> chipIns chip <> chipOuts chip)) . fst)
        (M.toList $ pinTable v)

testChip :: IO ()
testChip = do
  chip <- loadChip "ALU"
  eval
    chip
    (M.fromList
       [ ("x", Bus 16 5)
       , ("y", Bus 16 10)
       , ("zx", Bit 1)
       , ("nx", Bit 1)
       , ("zy", Bit 1)
       , ("ny", Bit 1)
       , ("f", Bit 1)
       , ("no", Bit 0)])