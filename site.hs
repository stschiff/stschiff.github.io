{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           BibTeX                 (BibEntry (..), BibTeX, readBibFile)
import           Data.Monoid            (mappend)
import           Hakyll

import           Control.Monad          (filterM, forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary            (Binary)
import           Data.List              (intercalate, sortOn)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Time              (UTCTime (..), addUTCTime,
                                         defaultTimeLocale, fromGregorian,
                                         getCurrentTime, nominalDay,
                                         secondsToDiffTime)
import           Debug.Trace            (trace)
import           GHC.Generics           (Generic)
import           System.FilePath.Posix  (replaceExtension, takeFileName)
import           Text.Parsec            (parse)

main :: IO ()
main = readBibFile "data/publications.bib" >>= runHakyll

runHakyll :: BibTeX -> IO ()
runHakyll bibEntries = hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/publications/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "data/pdfs/*" $ do
        route idRoute
        compile copyFileCompiler

    create ["sidebar"] . compile $ do
        posts <- loadAllSnapshots "posts/*" "raw"
        pubs <- loadAllSnapshots "pubs/*" "raw"
        
        let sidebarField = listField "sidebarItems" postCtx (return posts)
        makeItem "" >>= loadAndApplyTemplate "templates/sidebar.html" sidebarField

    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            -- this Snapshot is necessary to break a dependency cycle: sidebar requires posts which require sidebar.
            getResourceBody >>= saveSnapshot "raw"
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    create [fromFilePath . ("pubs/"++) . bibEntryId $ b | b <- bibEntries] $ do
        route $ setExtension "html"
        compile $ do
            id_ <- getUnderlying
            let citekey = drop 5 . toFilePath $ id_
            let [bibEntry] = filter ((== citekey) . bibEntryId) bibEntries
            makeItem (bibEntryType bibEntry, bibEntryId bibEntry, bibEntryFields bibEntry) >>= saveSnapshot "bibEntry"
            sidebarCtx <- loadSidebarContext
            makeItem bibEntry
                >>= loadAndApplyTemplate "templates/publication.html" getPubCtx
                >>= saveSnapshot "raw"
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            allPubEntries <- loadPubs 
            sidebarCtx <- loadSidebarContext
            let ctx = listField "publications" getPubCtx (return allPubEntries)
            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" (constField "title" "Publications" <> constField "menu_Publications" "True" <> sidebarCtx)
                >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let blogCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext
            sidebarCtx <- loadSidebarContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/base.html" (constField "title" "Blog" <> constField "menu_Blog" "True" <> sidebarCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

loadPubs :: Compiler [Item BibEntry]
loadPubs = do
    allPubEntryTupleItems <- loadAllSnapshots "pub/*" "bibEntry"
    forM allPubEntryTupleItems $ \ti -> do 
        let (t, i, f) = itemBody ti
        makeItem $ BibEntry t i f

loadSidebarContext :: Compiler (Context String)
loadSidebarContext = do
    sidebar <- itemBody <$> load "sidebar"
    return $ constField "sidebar" sidebar <> defaultContext

getPubCtx :: Context BibEntry
getPubCtx =
    makeBibField "title" <>
    makeSourceField "source" <>
    field "published" (return . makeBibTexDateField . itemBody) <>
    makeBibField "author" <>
    field "citekey" (return . bibEntryId . itemBody) <>
    makeBibField "url" <>
    makeBibField "abstract"
  where
    makeBibField :: String -> Context BibEntry
    makeBibField key = field key (maybeCompiler key)
    makeSourceField :: String -> Context BibEntry
    makeSourceField key = field key (\item ->
        case lookup "journal" . bibEntryFields . itemBody $ item of
            Just j -> return j
            Nothing -> case lookup "booktitle" . bibEntryFields . itemBody $ item of
                Just b -> return b
                Nothing -> noResult $ "bibEntry " ++ (bibEntryId . itemBody $ item) ++
                    " does not have fields journal or booktitle ")
    maybeCompiler :: String -> Item BibEntry -> Compiler String
    maybeCompiler key item =
        case lookup key . bibEntryFields . itemBody $ item of
            Just res -> return res
            Nothing -> noResult $ "bibEntry " ++ (bibEntryId . itemBody $ item) ++ " does not have field " ++ key

makeBibTexDate :: BibEntry -> UTCTime
makeBibTexDate b =
    let y = read . fromJust . lookup "year" . bibEntryFields $ b
        m = fromJust . lookup "month" . bibEntryFields $ b
        d = maybe 1 read . lookup "day" . bibEntryFields $ b
    in  UTCTime (fromGregorian y (bibTexMonthToNum m) d) 0
  where
    bibTexMonthToNum m = fromJust . lookup m $ monthNums
    monthNums = zip ["jan", "feb", "mar", "apr", "may", "jun",
                     "jul", "aug", "sep", "oct", "nov", "dec"] [1..]

makeBibTexDateField :: BibEntry -> String
makeBibTexDateField bibEntry =
    let maybeDateFields = [lookup f (bibEntryFields bibEntry) | f <- ["year", "month", "day"]]
    in  case sequence maybeDateFields of
            Just [year, month, day] -> bibTexMonthToStr month ++ " " ++ day ++ ", " ++ year
            Nothing ->
                case sequence (take 2 maybeDateFields) of
                    Just [year, month] -> bibTexMonthToStr month ++ ", " ++ year
                    Nothing -> error $ "could not find date information in BibEntry " ++ bibEntryId bibEntry
  where
    bibTexMonthToStr m = case m of
        "jan" -> "January"
        "feb" -> "February"
        "mar" -> "March"
        "apr" -> "April"
        "may" -> "May"
        "jun" -> "June"
        "jul" -> "July"
        "aug" -> "August"
        "sep" -> "September"
        "oct" -> "October"
        "nov" -> "November"
        "dec" -> "December"
        _     -> error $ "Could not parse BibTex month" ++ m

postCtx :: Context String
postCtx =
    boolField "pub" isPub <>
    boolField "post" isPost <>
    field "itemName" itemName <>
    field "post_url" postUrl <>
    dateField "date" "%B %e, %Y" <>
    defaultContext
  where
    isPost = (=="posts/") . take 6 . toFilePath . itemIdentifier
    isPub = (=="publications/") . take 13 . toFilePath . itemIdentifier
    itemName = return . takeFileName . toFilePath . itemIdentifier
    postUrl = return . ("/"<>) . (\fp -> replaceExtension fp "html") . toFilePath . itemIdentifier
