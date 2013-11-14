{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad

defContext :: Context String
defContext =
    mathCtx
    `mappend`  defaultContext


main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match (fromList ["contact.markdown", "links.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls

    match (fromList ["resume.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/res.html" defContext
            >>= relativizeUrls

    let postLike = \template -> do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ template ++ ".html") postCtx
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls

    let kinds = ["post", "project", "concept"]
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
                    listField "posts" postCtx (return posts) `mappend`
                    listField "projects" postCtx (return projects) `mappend`
                    constField "title" "Home"                `mappend`
                    defContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    mathCtx `mappend`
    defaultContext


pandocOptions :: WriterOptions
pandocOptions = d { writerExtensions = S.union (writerExtensions d) multimarkdownExtensions}
    where d = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                  then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                  else ""
