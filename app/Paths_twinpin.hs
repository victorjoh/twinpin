module Paths_twinpin where

-- override default behavior for loading data files. This will look in the
-- installation directory for data files instead of the .stack-work directory.
-- When running from source it will look in the top repository directory.
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
