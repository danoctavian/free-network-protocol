{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.NetworkProtocol
import Data.ByteString as BS

 runProtocol sendOp recvOp = run where
  run (Recv x f) = recvOp >>= run . f
  run (Send bs f) = (sendOp bs >> return 1) >>= run . f
  run (Finalize r) = sendOp "we're done" >> (return $ Right r)
  run (Exception e) = sendOp " EXCEPTION" >> (return $ Left e)


test = runProtocol BS.putStrLn BS.getLine (send "write bitch" >> recv 1 >>= send >> send "cl    osing up now" >> finalize 25)

