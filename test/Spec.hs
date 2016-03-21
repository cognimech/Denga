import Connectors.Transaq.FFI
import Connectors.Transaq

main :: IO ()
main = do
  ret <- getServiceInfo "<request><value>queue_size</value><value>queue_mem_used</value><value>version</value></request>"
  print ret
  ret <- initialize "C:\\Users\\Leo\\Documents\\Haskell\\hTransaq\\log\\" 2
  print ret
  ret <- unInitialize
  print ret
   {-serviceInfo >>= print
  initialize "C:\\Users\\Leo\\Documents\\Haskell\\hTransaq\\log\\" 2 >>= print
  connectWith $ do  
  unInitialize >>= print
  print ret
  ret <- disconnect
  print ret
  ret <- unInitialize
  print ret-}