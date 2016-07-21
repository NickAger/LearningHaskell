module HttpStuff where
  
import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.com/bytes/g"
       ]
       
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- better than the above
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls



