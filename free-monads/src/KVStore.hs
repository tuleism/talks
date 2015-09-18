{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module KVStore where

import Control.Monad.Free
import Control.Monad.Free.TH

import qualified Control.Monad.State as S

import Data.Map (Map)
import qualified Data.Map as M

type Key = String
type Value = String

data Operation next
  = Get Key (Maybe Value -> next)
  | Put Key Value next
  | Modify Key (Value -> Value) next
  deriving (Functor)

type OperationM = Free Operation
makeFree ''Operation

actions :: OperationM (Maybe Value)
actions = do
  put "key3" "value3"
  key3 <- get "key3"
  case key3 of
    Nothing -> put "key4" "value4"
    _ -> put "key5" "value5"
  get "key4"

type MockDB = Map String String

mockDb :: S.MonadState MockDB m => OperationM a -> m a
mockDb = iterM $ \case
  Get k f -> S.gets (M.lookup k) >>= f
  Put k v n -> S.modify (M.insert k v) >> n
  Modify k f n -> S.modify (M.adjust f k) >> n

mockIO :: OperationM a -> IO a
mockIO = iterM $ \case
  Get k f -> do
    putStr $ "Getting value for " ++ k ++ ": "
    value <- getLine
    f (Just value)
  Put k v n -> do
    putStrLn $ "Putting " ++ v ++ " into " ++ k
    n
  Modify k _ n -> do
    putStrLn $ "Modifying " ++ k
    n

main :: IO ()
main = do
  -- finalValue <- mockIO actions
  -- print finalValue
  print $ S.execState (mockDb actions) M.empty
