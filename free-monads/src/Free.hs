{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Free where

import Control.Monad.Free
import Control.Monad.Free.TH

data Action next
  = Receive (String -> next)
  | Response String next
  | End
  deriving (Functor)

type ActionM = Free Action
makeFree ''Action

actions :: ActionM ()
actions = do
  name <- receive
  response name
  end

consoleApp :: ActionM () -> IO ()
consoleApp = iterM $ \case
  Receive f -> getLine >>= f
  Response msg n -> putStrLn msg >> n
  End -> return ()

main :: IO ()
main = consoleApp actions
