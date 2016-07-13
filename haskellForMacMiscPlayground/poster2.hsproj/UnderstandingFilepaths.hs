module UnderstandingFilepaths where
  
getDataFileName :: FilePath -> IO FilePath
--getDataFileName name = do
--  dir <- return "/Users/nickager/tlcposter2/img"
--  return (dir ++ "/" ++ name)
  
getDataFileName name = return ("/Users/nickager/tlcposter2/img" ++ "/" ++ name)
  

