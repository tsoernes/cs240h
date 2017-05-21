-- | Run ParseIni as a web server
{-# LANGUAGE OverloadedStrings #-}
module Main where

import ParseIni
import PrettyPrintIni

import Data.ByteString.Builder
import qualified Data.ByteString as B
import Data.List (foldl', intersperse)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Happstack.Lite
import Text.Blaze.Html (ToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- *** HTML Generating functions *** --

-- |Take some HTML and put it in a monospaced blockquote.
monoBlock :: ToMarkup a => a -> H.Html
monoBlock = (H.blockquote ! A.style "font-family:monospace;") . H.pre . H.toHtml

-- |Equivalent of @<br clear="all"/>@ from days of yore.
brClear :: H.Html
brClear = H.br ! A.style "clear:both;"

-- |A skeleton HTML response.
pageSkeleton :: H.Html -> H.Html
pageSkeleton contents = H.docTypeHtml $ do
    H.head $ do
        H.title "Stanford CS240h Lab 2 Oracle"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css"
               ! A.href "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
        H.link ! A.rel "shortcut icon" ! A.href "http://www.scs.stanford.edu/favicon.ico"
    H.body $ do
        H.article ! A.class_ "container" $ do
            H.h1 "Stanford CS240h Lab 2 Oracle"
            H.p "Questions about the spec? Ask the oracle!"
            contents

-- |HTML for a form requesting input from the user.
submitForm :: T.Text -> H.Html
submitForm t = do
    H.form ! A.action "/form" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
        H.textarea ! A.name "iniFile" ! A.rows "25" ! A.cols "100"
                   ! A.style "font-family:monospace;"
                   ! A.placeholder "Your input goes here" $
                   H.toHtml t
        brClear
        H.input ! A.type_ "submit" ! A.value "Submit"

-- |Render the user's input ByteString as HTML.
showInput :: B.ByteString -> H.Html
showInput i = do
    H.h3 "Input"
    H.p "Your input was:"
    monoBlock $ show i




-- *** Handlers for GET and POST *** --

-- |After receiving user input, if a parse error occurs, this is the page we generate.
showError :: B.ByteString -> T.Text -> String -> H.Html
showError i t e = pageSkeleton $ do
    showInput i
    H.h3 ! A.style "color:red;" $ "Parse error"
    H.p "The parser returned the following error (but note that it is probably not meaningful!):"
    monoBlock e
    brClear
    submitForm t

-- |After receiving user input, if a parse is successful, this is the page we generate.
showResult :: B.ByteString -> T.Text -> INIFile -> H.Html
showResult i t f = pageSkeleton $ do
    showInput i
    H.h3 "Parser results"
    H.p "The parser returned the following INIFile:"
    monoBlock $ showINIFile f
    H.h3 "Pretty printer results"
    H.p "The pretty printer returned the following text:"
    monoBlock . decodeUtf8 $ prettyPrint f
    brClear
    submitForm t

-- |After receiving user input, attempt a parse, and then call the appropriate handler.
postResponse :: ServerPart Response
postResponse = do method POST
                  iniFileText <- lookText "iniFile"
                  let iniFile = encodeUtf8 . T.toStrict . removeLF $ iniFileText
                  let iniParsed = parseIniFile iniFile
                  let sErr = showError iniFile iniFileText
                  let sRes = showResult iniFile iniFileText
                  ok . toResponse $ either sErr sRes iniParsed

-- |Before receiving user input, display a form requesting some input.
getResponse :: ServerPart Response
getResponse = method GET >> ( ok . toResponse . pageSkeleton $ submitForm T.empty )




-- *** Prettified `show` for INIFile *** ---

-- |A pretty printer for INIFile that gives an output like the Show instance of Map.
showINIFile :: INIFile -> T.Text
showINIFile = TE.decodeUtf8 . toLazyByteString . M.foldrWithKey showINIHelp mempty
  where showINIHelp k s m = showSectName k <> showSection s <> byteString "\n\n" <> m

-- |Show a section name in a prettier "show"-ish style.
showSectName :: INISectName -> Builder
showSectName n = stringUtf8 (show n) <> byteString "\n"

-- |Show a section in a prettier "show"-ish style.
showSection :: INISection -> Builder
showSection s = byteString "  [ " <> kvWithSeps <> byteString "\n  ]"
  where kvWithSeps = foldl' (<>) mempty $ intersperse (byteString "\n  , ") kvShows
        kvShows = map showKV $ M.assocs s
        showKV (k, vs) = stringUtf8 (show k) <> byteString "\n      [ " <> vWithSeps vs <> byteString "\n      ]"
        vWithSeps vs = foldl' (<>) mempty $ intersperse (byteString "\n      , ") $ vShows vs
        vShows vs = map (stringUtf8 . show) vs




-- *** Misc *** --

-- |Remove linefeed characters (@\r@) from the input stream.
--
-- We generally assume that line endings are in UNIX format, but web
-- browsers generally use Windows-ish line endings (i.e., "\r\n").
-- To mitigate this, we just strip all linefeeds out of the response.
removeLF :: T.Text -> T.Text
removeLF = T.filter $ \c -> c /= '\r'

-- |Generic configuration for the server.
parserServerConfig :: ServerConfig
parserServerConfig = ServerConfig { port = 8888
                                  , ramQuota = 65535
                                  , diskQuota = 0
                                  , tmpDir = "/dev/null"
                                  }

-- |Main - kick off the web server
main :: IO ()
main = serve (Just parserServerConfig) $ msum [getResponse, postResponse]
