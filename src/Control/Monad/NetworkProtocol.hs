{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.NetworkProtocol where
import Prelude as P

import Data.ByteString as BS
import Data.Serialize
import Control.Applicative

type ExceptionValue = String

data Protocol result = Send ByteString (Protocol result)
                     | Recv Int (ByteString -> Protocol result)
                     | forall a . (Serialize a) => RecvMsg  (a -> Protocol result)
                     | forall a . (Serialize a) => SendMsg a (Protocol result)
                     | Finalize result
                     | Exception ExceptionValue

-- operations
send bs = Send bs $ return ()
recv sz = Recv sz return
finalize = Finalize 
recvMsg :: forall a . (Serialize a) => Protocol a
recvMsg = RecvMsg return
sendMsg :: forall a . (Serialize a) => a -> Protocol ()
sendMsg m = SendMsg m $ return ()
throwException = Exception

-- making it a free monad.
-- cannot use the 'free' package because of the existential types in Protocol
instance Functor Protocol where
  fmap f (Finalize r) = Finalize  $ f r
  fmap f (Exception err) = Exception err
  fmap f (Send bs g) = Send bs (fmap f $ g)
  fmap f (Recv s g) = Recv s (fmap f . g)
  fmap f (SendMsg m g) = SendMsg m (fmap f $ g)
  fmap f (RecvMsg g) = RecvMsg (fmap f . g)

instance Applicative Protocol where
  pure = return
  (<*>) = undefined

instance Monad Protocol where
  return = Finalize
  Finalize r >>= m = m r
  Exception e >>= m = Exception e
  Send bs p >>= m = Send bs (p >>= m)
  Recv sz p >>= m = Recv sz (\bs -> p bs >>= m)
  SendMsg msg p >>= m = SendMsg msg (p >>= m)
  RecvMsg p >>= m = RecvMsg (\bs -> p bs >>= m)

runProtocol sendOp recvOp = run where
  run (Recv x f) = recvOp >>= run . f
  run (Send bs f) = sendOp bs >> run f
  run (SendMsg msg f) = (sendOp (encode msg)) >> run f
  run (Finalize r) = sendOp "we're done" >> (return $ Right r)
  run (Exception e) = sendOp " EXCEPTION" >> (return $ Left e)


test = runProtocol BS.putStrLn BS.getLine (send "write bitch" >> recv 1 >>= send >> send "closing up now" >> finalize 25)
