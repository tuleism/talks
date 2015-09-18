{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module OurFree where

import Control.Applicative

data Action next
  = Receive (String -> next)
  | Response String next
  | End
  deriving (Functor)

data Free f a = Free (f (Free f a)) | Pure a

instance (Functor f) => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free f >>= g = Free $ fmap (>>= g) f

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

type ActionM = Free Action

liftF :: (Functor f) => f r -> Free f r
liftF = Free . fmap Pure

iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a
iterM _   (Pure x) = return x
iterM phi (Free f) = phi $ fmap (iterM phi) f

receive :: ActionM String
receive = liftF $ Receive id

response :: String -> ActionM ()
response msg = liftF $ Response msg ()

end :: ActionM ()
end = liftF End

actions :: ActionM ()
actions = do
  name <- receive
  response name
  end

-- consoleApp :: ActionM () -> IO ()
-- consoleApp = iterM $ \case
--   Receive f -> getLine >>= f
--   Response msg n -> putStrLn msg >> n
--   End -> return ()

consoleIntepreter :: ActionM () -> IO ()
consoleIntepreter = \case
  Pure x -> return x
  Free (Receive f) -> getLine >>= consoleIntepreter . f
  Free (Response msg n) -> putStrLn msg >> consoleIntepreter n
  Free End -> return ()

main :: IO ()
main = consoleIntepreter actions
