module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Lens
import Text.Printf
import System.Environment
import System.FilePath
import System.Directory
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

indexFiles :: [String]
indexFiles = ["index.html", "index.htm"]

renderLink :: FilePath -> FilePath -> String
renderLink target filename = printf "<div><a href=%s>%s</a></div>" href filename
    where href = '/' : target </> filename

main :: IO ()
main = do
    basepath <- view (pre _head . non ".") <$> getArgs

    putStrLn "Server is running on port 1509"
    run 1509 $ \req resp -> do
        runExceptT >=> resp . either id id $ do
            let target = joinPath $ pathInfo req ^.. traverse . unpacked
                path = basepath </> target

            exists <- liftIO $ doesPathExist path
            unless exists $ throwError $ responseLBS notFound404 [] $ "No such file or directory" ^. packedChars

            dir <- liftIO $ doesDirectoryExist path
            if dir then do
                liftIO $ print path
                forM_ (map (path</>) indexFiles) $ \indexPath -> do
                    liftIO $ print indexPath
                    exists <- liftIO $ doesPathExist indexPath
                    when exists $ throwError $ responseFile ok200 [] indexPath Nothing

                files <- liftIO $ listDirectory path
                pure $ responseLBS ok200 [] $ files ^. traverse . to (renderLink target) . packedChars
            else do
                pure $ responseFile ok200 [] path Nothing
