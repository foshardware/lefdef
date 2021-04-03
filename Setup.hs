
import Control.Monad

import Distribution.Simple

import System.IO
import System.Environment
import System.Process



main :: IO ()
main = do
  args <- getArgs
  when ("--enable-tests" `elem` args) $ pullSubmodules
  defaultMain



pullSubmodules :: IO ()
pullSubmodules = git ["submodule", "update", "--init", "--recursive"] putStr


git :: [String] -> (String-> IO ()) -> IO ()
git xs sink  = withCreateProcess
  (proc "git" xs) { std_out = CreatePipe }
  (\ _ (Just i) _ _ -> sink =<< hGetContents i)

