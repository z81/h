{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative            ( (<$>) )
import           Control.Monad
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types             ( status200
                                                , status404
                                                , status403
                                                , status500
                                                )
import           Network.HTTP.Types.Header      ( hContentType )
import           Blaze.ByteString.Builder.Char.Utf8
                                                ( fromString )
import           Data.ByteString                ( ByteString )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.List
import           Blaze.ByteString.Builder       ( copyByteString )
import           System.Environment
import           Data.Text                      ( pack
                                                , Text
                                                )
import           Data.ByteString.Char8          ( unpack )
import           Text.Read                      ( readMaybe )
import           System.Process
import           System.Exit                    ( ExitCode
                                                    ( ExitFailure
                                                    , ExitSuccess
                                                    )
                                                )

data AppConfig = AppConfig {
  port :: Int,
  token :: Text,
  dockerComposeRootPath :: Text
} deriving (Show, Eq)

getAppConfig :: IO AppConfig
getAppConfig = do
    portResult              <- lookupEnv "PORT"
    tokenResult             <- lookupEnv "TOKEN"
    dockerComposePathResult <- lookupEnv "DOCKER_COMPOSE_PATH"
    case (portResult, tokenResult, dockerComposePathResult) of
        (Just port, Just token, Just dockerComposePath) ->
            case readMaybe port :: Maybe Int of
                Nothing      -> error "PORT isn't a number"
                Just portNum -> return $ AppConfig portNum
                                                   (pack token)
                                                   (pack dockerComposePath)
        (Nothing, _      , _      ) -> error "Couldn't find PORT"
        (_      , Nothing, _      ) -> error "Couldn't find TOKEN"
        (_      , _      , Nothing) -> error "Couldn't find DOCKER_COMPOSE_PATH"



main :: IO ()
main = do
    config <- getAppConfig
    let port = AppConfig port
    putStrLn $ "Listening on port " ++ show port
    run port app

-- app :: Request -> Response -> IO Response
app req respond = do
    response <- case pathInfo req of
        ["hook"] -> hook req
        _        -> notFound
    respond response

getHTTPQueryParam query name = case join $ lookup name query of
    Nothing  -> ""
    Just val -> decodeUtf8 val

runApp appName dockerComposePathValue = do
    (exitcode', stdout', stderr') <- readProcessWithExitCode
        appName
        [dockerComposePathValue]
        ""
    return exitcode'


hook req = do
    config <- getAppConfig
    let query = queryString req :: [(ByteString, Maybe ByteString)]
    let applicationToken       = AppConfig token
    let dockerComposePathValue = AppConfig dcPath
    let tokenParam             = getHTTPQueryParam query "token" :: Text
    let appnameParam           = getHTTPQueryParam query "appname" :: Text

    case tokenParam == applicationToken of
        True -> do
            let path     = unpack $ encodeUtf8 dockerComposePathValue
            let name     = unpack $ encodeUtf8 appnameParam
            let filename = name ++ ".yaml"
            let cmd =
                    "docker-compose -f "
                        ++ filename
                        ++ " pull && docker-compose -f "
                        ++ filename
                        ++ " restart"
                        ++ name
            code <- runApp cmd path
            case code of
                ExitSuccess ->
                    return
                        $ responseBuilder status200
                                          [(hContentType, "text/plain")]
                        $ fromString "Ok"
                _ ->
                    return
                        $ responseBuilder status500
                                          [(hContentType, "text/plain")]
                        $ fromString "err"
        False -> do
            return
                $ responseBuilder status403 [(hContentType, "text/plain")]
                $ fromString
                $ "Invalid token"


notFound = do
    return
        $ responseBuilder status404 [("Content-Type", "text/plain")]
        $ mconcat
        $ map copyByteString ["Not found"]
