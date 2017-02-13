{-# LANGUAGE OverloadedStrings #-}
-- import           Data.Monoid (mappend,mconcat)
import           Hakyll
import           Text.Pandoc.Options
-- import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad
import           Data.String(IsString(..))
-- import           Debug.Trace

recentPostCount :: Int
recentPostCount = 5

myConfig :: Configuration
myConfig = defaultConfiguration
    { deployCommand = "rsync -avz _site/ levischuck.com:/var/www/levischuck/"
    }

kinds :: (IsString a, IsString b) => [(String, a, a, [(b, Maybe b)])]
kinds =
  [ ( "Posts"
    , "post"
    , "posts"
    , [("/*", Nothing)]
    )
  , ( "Projects"
    , "project"
    , "projects"
    , [ ("/*", Nothing)
      , ("/*/*", Just "/*/index.*")
      , ("/*/*/*", Just "/*/*/index.*")
      ]
    )
  , ( "Concepts"
    , "concept"
    , "concepts"
    , [ ("/*", Nothing)
      , ("/*/*", Just "/*/index.*")
      ]
    )
  , ( "Pages"
    , "page"
    , "pages"
    , [ ("/*", Nothing)
      , ("/*/*", Just "/*/index.*")
      ]
    )
  , ( "Chats"
    , "chat"
    , "chats"
    , [ ("/*", Nothing)
      , ("/*/*", Just "/*/index.*")
      ]
    )
  ]

foldBody :: [Item String] -> Compiler (Item String)
foldBody xs = makeItem $ concat $ map itemBody xs

main :: IO ()
main = hakyllWith myConfig $ do
    let idCopy = do
        route   idRoute
        compile copyFileCompiler
    match "images/*" idCopy
    match "images/*/*" idCopy
    match "css/*.css" $ idCopy

    match "css/*.hs" $ do
      route   $ setExtension "css"
      compile $ getResourceString >>= withItemBody (unixFilter "cabal" ["exec", "--", "runghc"])
    match "graphs/*.dot" $ do
      version "svg" $ do
        route   $ setExtension "svg"
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS "dot" ["-Tsvg"])
      version "png" $ do
        route   $ setExtension "png"
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS "dot" ["-Tpng"])

    let htmlWith = \template -> do
        route   $ setExtension "html"
        compile $ pandocCompilerWith readerOptions pandocOptions
            >>= loadAndApplyTemplate template defContext
            >>= relativizeUrls

    match
      (fromList ["contact.markdown", "links.markdown"])
      (htmlWith "templates/default.html")

    match
      (fromList ["resume.markdown"])
      (htmlWith "templates/res.html")


    let postLike = \template -> do
        route $ setExtension "html"
        compile $ pandocCompilerWith readerOptions pandocOptions
            >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ template ++ ".html") postCtx
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls
    let indexLike = \template c i -> do
        let pat = complement i .&&. c
        route $ setExtension "html"
        compile $ do
          children <- loadAll pat
          let ctx = listField "items" postCtx (return children) `mappend` postCtx
          pandocCompilerWith readerOptions pandocOptions
            >>= loadAndApplyTemplate (fromFilePath $ "templates/index-" ++ template ++ ".html") ctx
            >>= loadAndApplyTemplate "templates/default.html" defContext
            >>= relativizeUrls

    forM_ kinds $ \(_, t, ts, p) -> do
      forM_ p $ \(pat, rep) -> do
        let cpat = fromGlob $ ts ++ pat
        case rep of
          Nothing -> return ()
          Just r -> do
            let ipat = fromGlob $ ts ++ r
            match ipat $ indexLike t cpat ipat
        match cpat $ postLike t

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            archive' <- forM kinds $ \(n, _, ts, p) -> do
              let isPosts = ts == "posts"
              items' <- forM p $ \(pat, indexed) -> do
                let pat' = case indexed of
                      Nothing -> pat
                      Just idx -> idx
                members' <- loadAll $ fromGlob $ ts ++ pat'
                if isPosts
                  then recentFirst members'
                  else return members'
              let items = concat items'
              let ctx = mconcat
                    [ listField "items" postCtx (return items)
                    , constField "archivetitle" n
                    , defContext
                    ]
              makeItem "" >>= if isPosts
                then loadAndApplyTemplate "templates/post-list.html" ctx
                else loadAndApplyTemplate "templates/archive.html" ctx
            archiveMerge <- foldBody archive'
            let archive = Item
                  { itemIdentifier = fromFilePath "archive.html"
                  , itemBody = itemBody archiveMerge
                  }
            let archiveCtx = constField "title" "Archive" `mappend` defContext
            return archive
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return (take recentPostCount posts)) `mappend`
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
desiredExtensions = wanted
    where
        wanted = S.fromList
            [ Ext_line_blocks
            , Ext_autolink_bare_uris
            , Ext_footnotes
            , Ext_grid_tables
            ]

readerOptions :: ReaderOptions
readerOptions = d { readerExtensions = re}
    where
        d = defaultHakyllReaderOptions
        re = S.union (readerExtensions d) desiredExtensions

pandocOptions :: WriterOptions
pandocOptions = d { writerExtensions = we}
    where
        d = defaultHakyllWriterOptions
          { writerHTMLMathMethod = MathJax ""
          , writerEmailObfuscation = ReferenceObfuscation
          }
        we = S.union (writerExtensions d) desiredExtensions


mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
  let identifier = itemIdentifier item
  meta <- getMetadataField identifier "mathjax"
  case meta of
    Nothing -> return ""
    Just _ -> return "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"

tweetCtx :: Context a
tweetCtx = field "tweets" $ \item -> do
  let identifier = itemIdentifier item
  meta <- getMetadataField identifier "tweets"
  case meta of
    Nothing -> return ""
    Just _ -> return "<script async src=\"//platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"
