import System.IO
import System.Directory
import System.Environment (getArgs)
import Control.Monad.Extra (ifM)

import Server
import Console

main :: IO ()
main = hSetBuffering stdin (BlockBuffering Nothing) >> ifM (doesDirectoryExist ".databases") (return ()) (createDirectory ".databases") >> ifM ((== ["server"]) <$> getArgs) server console
