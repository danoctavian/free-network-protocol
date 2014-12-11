module Control.Monad.NetworkProtocol.Conduit where

import Data.Conduit as DC
import Data.Conduit.Cereal
import Data.Conduit.Binary as CB
import Data.ByteString.Lazy (toStrict)
import qualified Data.Conduit.Internal as DCI
import Control.Monad.Catch
import Data.Serialize
import Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)

import Control.Monad.NetworkProtocol

conduitProtocol :: (Monad m, MonadThrow m) => Protocol b
                  -> ConduitM ByteString ByteString m (Either ExceptionValue b)
conduitProtocol = run where
  run (Recv x f) = (fmap toStrict $ CB.take x) >>= run . f
  run (Send bs f) = yield bs >> run f
  run (RecvMsg f) = (sinkGet get) >>= run . f
  run (SendMsg msg f) = yield (encode msg) >> run f
  run (Finalize r) = return $ Right r
  run (Exception e) = return $ Left e

runProtocol src dest protocol = src $$+ (fuseBoth (conduitProtocol protocol) dest)
