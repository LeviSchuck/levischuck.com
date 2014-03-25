{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad

recentPostCount :: Int
recentPostCount = 5

myConfig :: Configuration
myConfig = defaultConfiguration
    { deployCommand = "rsync -avz _site/ levischuck.com:/var/www/levischuck/"
    }


main :: IO ()
main = hakyllWith myConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "images/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])
    match "graphs/*.dot" $ do
        route   $ setExtension "svg"
        compile $ getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])
    match "graphs/*.dot" $ do
        route   $ setExtension "png"
        compile $ getResourceString >>= withItemBody (unixFilter "dot" ["-Tpng"])

    match (fromList ["contact.markdown", "links.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls

    match (fromList ["resume.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith readerOptions pandocOptions
            >>= loadAndApplyTemplate "templates/res.html" defContext
            >>= relativizeUrls

    let postLike = \template -> do
        route $ setExtension "html"
        compile $ pandocCompilerWith readerOptions pandocOptions
            >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ template ++ ".html") postCtx
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls

    let kinds = ["post", "project", "concept", "page", "chat"]
    forM_ kinds $ \k -> do
        match (fromGlob $ k ++ "s/*") $ postLike k


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- loadAll "projects/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "projects" postCtx (return projects) `mappend`
                    constField "title" "Archives"            `mappend`
                    defContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- loadAll "projects/*"
            let indexCtx =
                    listField "posts" postCtx (return (take recentPostCount posts)) `mappend`
                    listField "projects" postCtx (return projects) `mappend`
                    constField "title" "Home"                `mappend`
                    defContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

defContext :: Context String
defContext =
    mathCtx
    `mappend` tweetCtx
    `mappend`  defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

desiredExtensions :: S.Set Extension
desiredExtensions = wanted -- S.union multimarkdownExtensions wanted
    where
        wanted = S.fromList
            [ Ext_line_blocks
            , Ext_autolink_bare_uris
            , Ext_footnotes
            -- , Ext_pipe_tables
            -- , Ext_implicit_figures
            -- , Ext_link_attributes
            ]

readerOptions :: ReaderOptions
readerOptions = d { readerExtensions = re}
    where
        d = defaultHakyllReaderOptions
        re = S.union (readerExtensions d) desiredExtensions



pandocOptions :: WriterOptions
pandocOptions = d { writerExtensions = we}
    where
        d = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }
        we = S.union (writerExtensions d) desiredExtensions


mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                  then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                  else ""
tweetCtx :: Context a
tweetCtx = field "tweets" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if M.member "tweets" metadata
        then "<script async src=\"//platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"
        else ""