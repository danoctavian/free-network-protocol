{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.NetworkProtocol where
import Prelude as P

import Data.ByteString as BS
import Data.Serialize
import Control.Applicative

type ExceptionValue = String

data Protocol e result = Send ByteString (Protocol e result)
                     | Recv Int (ByteString -> Protocol e result)
                     | forall a . (Serialize a) => RecvMsg  (a -> Protocol e result)
                     | forall a . (Serialize a) => SendMsg a (Protocol e result)
                     | Finalize result
                     | Exception e 

-- operations
send bs = Send bs $ return ()
recv sz = Recv sz return
finalize = Finalize 
recvMsg :: forall e a . (Serialize a) => Protocol e a
recvMsg = RecvMsg return
sendMsg :: forall e a . (Serialize a) => a -> Protocol e ()
sendMsg m = SendMsg m $ return ()
throwException = Exception

-- making it a free monad.
-- cannot use the 'free' package because of the existential types in Protocol
instance Functor (Protocol e) where
  fmap f (Finalize r) = Finalize  $ f r
  fmap f (Exception err) = Exception err
  fmap f (Send bs g) = Send bs (fmap f $ g)
  fmap f (Recv s g) = Recv s (fmap f . g)
  fmap f (SendMsg m g) = SendMsg m (fmap f $ g)
  fmap f (RecvMsg g) = RecvMsg (fmap f . g)

instance Applicative (Protocol e) where
  pure = return
  (<*>) = undefined

instance Monad (Protocol e) where
  return = Finalize
  Finalize r >>= m = m r
  Exception e >>= m = Exception e
  Send bs p >>= m = Send bs (p >>= m)
  Recv sz p >>= m = Recv sz (\bs -> p bs >>= m)
  SendMsg msg p >>= m = SendMsg msg (p >>= m)
  RecvMsg p >>= m = RecvMsg (\bs -> p bs >>= m)

