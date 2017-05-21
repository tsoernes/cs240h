-- | Run MergeIni as a web server
{-# LANGUAGE OverloadedStrings #-}
module Main where

import MergeIni
import ParseIni
import PrettyPrintIni

import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import Happstack.Lite
import Text.Blaze.Html (ToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Take some HTML and put it in a monospaced blockquote.
monoBlock :: ToMarkup a => a -> H.Html
monoBlock = (H.blockquote ! A.style "font-family:monospace;") . H.pre . H.toHtml

-- | Equivalent of @<br clear="all"/>@ from days of yore.
brClear :: H.Html
brClear = H.br ! A.style "clear:both;"

-- | A skeleton HTML response.
pageSkeleton :: H.Html -> H.Html
pageSkeleton contents = H.docTypeHtml $ do
    H.head $ do
        H.title "Stanford CS240h Lab 3 Oracle"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css"
               ! A.href "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
        H.link ! A.rel "shortcut icon" ! A.href "http://www.scs.stanford.edu/favicon.ico"
    H.body $
        H.article ! A.class_ "container" $ do
            H.h1 "Stanford CS240h Lab 3 Oracle"
            H.p "Questions about the spec? Ask the oracle!"
            contents

-- | HTML for a form requesting input from the user.
submitForm :: T.Text -> H.Html
submitForm t =
    H.form ! A.action "/form" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
        H.textarea ! A.name "baseFile" ! A.rows "10" ! A.cols "80"
                   ! A.style "font-family:monospace;"
                   ! A.placeholder "Your base file goes here" $
                   H.toHtml t
        H.textarea ! A.name "mergeFile" ! A.rows "10" ! A.cols "80"
                   ! A.style "font-family:monospace;"
                   ! A.placeholder "Your merge file goes here" $
                   H.toHtml t
        H.textarea ! A.name "headFile" ! A.rows "10" ! A.cols "80"
                   ! A.style "font-family:monospace;"
                   ! A.placeholder "Your head file goes here" $
                   H.toHtml t
        brClear
        H.input ! A.type_ "submit" ! A.value "Submit"

-- | Render the user's input ByteString as HTML.
showInput :: B.ByteString -> H.Html
showInput i = do
    H.h3 "Input"
    H.p "Your input was:"
    monoBlock $ show i

-- | After receiving user input, if a parse error occurs, this is the page we generate.
showError :: B.ByteString -> T.Text -> String -> H.Html
showError i t e = pageSkeleton $ do
    showInput i
    H.h3 ! A.style "color:red;" $ "Parse error"
    H.p "The parser returned the following error (but note that it is probably not meaningful!):"
    monoBlock e
    brClear
    submitForm t

-- | After receiving user input, if a parse is successful, this is the page we generate.
showResult :: INIFile -> H.Html
showResult f = pageSkeleton $ do
    H.h3 "Merge Results"
    H.p "The merge driver returned the following ini file:"
    monoBlock . decodeUtf8 $ prettyPrint f
    brClear

-- | INI Files to parsed results
parseINIFiles :: [B.ByteString] -> Either H.Html [INIFile]
parseINIFiles = mapM parse
  where parse xs = case parseIniFile xs of
                    Left err -> Left $ showError xs "" err
                    Right f  -> Right f

-- | After receiving user input, attempt a parse, and then call the appropriate handler.
postResponse :: ServerPart Response
postResponse = do
  method POST
  baseFileT  <- lookText "baseFile"
  mergeFileT <- lookText "mergeFile"
  headFileT  <- lookText "headFile"
  let baseFile  = encodeUtf8 . T.toStrict . removeLF $ baseFileT
      mergeFile = encodeUtf8 . T.toStrict . removeLF $ mergeFileT
      headFile  = encodeUtf8 . T.toStrict . removeLF $ headFileT
      iniFiles  = parseINIFiles [baseFile, mergeFile, headFile]
  case iniFiles of
    Left html -> ok . toResponse $ html
    Right [baseI, mergeI, headI] -> do
      let mergeRes = mergeINI headI baseI mergeI
      ok . toResponse $ showResult mergeRes
    _ -> error "unmatched case statement"

  where
    -- |Remove linefeed characters (@\r@) from the input stream.
    --
    -- We generally assume that line endings are in UNIX format, but web
    -- browsers generally use Windows-ish line endings (i.e., "\r\n").
    -- To mitigate this, we just strip all linefeeds out of the response.
    removeLF = T.filter $ \c -> c /= '\r'

-- | Before receiving user input, display a form requesting some input.
getResponse :: ServerPart Response
getResponse = method GET >> (ok . toResponse . pageSkeleton $ submitForm T.empty)

-- | Main - kick off the web server.
main :: IO ()
main = serve (Just parserServerConfig) $ msum [getResponse, postResponse]
  where
    parserServerConfig = ServerConfig { port = 8888
                                      , ramQuota = 65535
                                      , diskQuota = 0
                                      , tmpDir = "/dev/null"
                                      }

