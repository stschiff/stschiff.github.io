{-# LANGUAGE OverloadedStrings #-}
import           BibTeX                 (BibTeX, BibEntry (..), readBibFile)
import           Data.Monoid            (mappend)
import           Hakyll

import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate)
import           Data.Maybe             (fromJust)
import           Data.Time              (UTCTime (..), addUTCTime,
                                         defaultTimeLocale, fromGregorian,
                                         getCurrentTime, nominalDay,
                                         secondsToDiffTime)
import           Debug.Trace            (trace)
import           System.FilePath.Posix  (replaceExtension, takeFileName)
import           Text.Parsec            (parse)

data SidebarType = SbAnnouncement | SbPost | SbPub

data SideBarItem = SidebarItem {
    sbType :: SidebarType,
    sbDate :: UTCTime,
    sbTitle :: String,
    sbImageFile :: FilePath,
    sbLink :: String
}

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
        posts <- loadAll "posts/*"
        let sbItems = sortSbItems $ map post2sbItem posts <> map pub2sbItem bibEntries
            sidebarField = listField "sidebarItems" sbItemContext sbItems
        makeItem "" >>= loadAndApplyTemplate "templates/sidebar.html" sidebarField


    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            baseCtx <- getBaseCtx Nothing
            ext <- getUnderlyingExtension
            let c = if ext == ".html" then getResourceBody else pandocCompiler
            c
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            baseCtx <- getBaseCtx Nothing
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    -- match "posts/*" $ version "raw" $ do
    --     compile getResourceString

    create (map (fromFilePath . bibEntryId) bibEntries) $ do
        route $ customRoute (\i -> "pub/" ++ toFilePath i ++ ".html")
        compile $ do
            id_ <- getUnderlying
            let [bibEntry] = filter ((== toFilePath id_) . bibEntryId) bibEntries
                abstract = fromJust . lookup "abstract" . bibEntryFields $ bibEntry 
            baseCtx <- getBaseCtx (Just "")
            makeItem bibEntry
                >>= loadAndApplyTemplate "templates/publication.html" getPubCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            baseCtx <- getBaseCtx (Just "Publications")
            let ctx = listField "publications" getPubCtx (mapM makeItem bibEntries)
            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let blogCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext
            baseCtx <- getBaseCtx (Just "Blog")
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/base.html" (baseCtx <> constField "menu_Blog" "True")
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

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

getBaseCtx :: BibTeX -> Maybe String -> Compiler (Context String)
getBaseCtx bibEntries maybeTitle = do
    currentTime <- unsafeCompiler getCurrentTime
    posts <- loadAll ("posts/*" .&&. hasVersion "raw")
    let pubs = bibEntries
    filteredSelectedPubs <- filterM isSelected pubs
    let sidebarField = listField "sidebarItems" postCtx (recentFirst $ posts <> pubs)
        titleField = case maybeTitle of
            Nothing -> mempty
            Just t  -> constField "title" t
    return $ sidebarField <> titleField <> defaultContext
  where
    isMoreRecentThan :: (MonadMetadata m) => UTCTime -> Item a -> m Bool
    isMoreRecentThan cutoffTime i = return True -- do
        -- itemTime <- getItemUTC defaultTimeLocale . itemIdentifier $ i
        -- return (itemTime > cutoffTime)

isSelected :: (MonadMetadata m) => Item a -> m Bool
isSelected i = do
    f <- getMetadataField (itemIdentifier i) "selected"
    case f of
        Just _  -> return True
        Nothing -> return False
