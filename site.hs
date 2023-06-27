{-# LANGUAGE OverloadedStrings #-}
import           BibTeX                 (BibTeX, BibEntry (..), readBibFile)
import           Data.Monoid            (mappend)
import           Hakyll

import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate, sortOn)
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
    sbDate :: Either (Int, Int) (Int, Int, Int),
    sbTitle :: String,
    sbImageFile :: Maybe FilePath,
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
        postSbItems <- mapM post2sbItem posts
        let sbItems = sortSbItems $ postSbItems ++ map pub2sbItem bibEntries
            sidebarField = listField "sidebarItems" sbItemContext (return sbItems)
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
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    create (map (fromFilePath . bibEntryId) bibEntries) $ do
        route $ customRoute (\i -> "pub/" ++ toFilePath i ++ ".html")
        compile $ do
            id_ <- getUnderlying
            let [bibEntry] = filter ((== toFilePath id_) . bibEntryId) bibEntries
                abstract = fromJust . lookup "abstract" . bibEntryFields $ bibEntry 
            sidebarCtx <- loadSidebarContext
            makeItem bibEntry
                >>= loadAndApplyTemplate "templates/publication.html" getPubCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            sidebarCtx <- loadSidebarContext
            let ctx = listField "publications" getPubCtx (mapM makeItem bibEntries)
            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx

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
                >>= loadAndApplyTemplate "templates/base.html" (sidebarCtx <> constField "menu_Blog" "True")
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

sortSbItems :: [Item SideBarItem] -> [Item SideBarItem]
sortSbItems = sortOn (sbDate . itemBody)

post2sbItem :: Item String -> Compiler (Item SideBarItem)
post2sbItem a = do
    let id_ = itemIdentifier a
    t <- getMetadataField' id_ "title"
    UTCTime date _ <- getItemUTC defaultTimeLocale id_
    i <- getMetadataField id_ "image"
    l <- getMetadataField' id_ "url"
    isBlogPost <- getMetadataField id_ "isBlogPost"
    let type_ = case isBlogPost of
            Just "True" -> SbPost
            _ -> SbAnnouncement
    maybeRoute <- getRoute id_
    url <- case maybeRoute of
        Nothing -> fail $ "No route url found for item " ++ show id_
        Just r -> return r
    makeItem $ SidebarItem type_ (Right (toGregorian date)) t i l

pub2sbItem :: BibEntry -> Compiler (Item SideBarItem)
pub2sbItem b = do
    let t = fromJust . lookup "title" . bibEntryFields $ b
        y = fromJust . lookup "year" . bibEntryFields $ b
        m = fromJust . lookup "month" . bibEntryFields $ b
        md = lookup "day" . bibEntryFields $ b
        date = case md of
            Nothing -> Left (y, m)
            Right d -> Right (y, m, d)
        citekey = bibEntryId b
        i = "images/publications/" ++ citekey ++ ".jpg"
        url = "pub/" ++ citekey ++ ".html"
    makeItem $ SidebarItem SbPub date t i url

sbItemContext :: Context SideBarItem
sbItemContext = undefined

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
    let y = fromJust . lookup "year" . bibEntryFields $ b
        m = fromJust . lookup "month" . bibEntryFields $ b
        d = maybe 1 . lookup "day" . bibEntryFields $ b
        date = UTCTime (fromGregorian y (bibTexMonthToNum m) d) 0
  where
    bibTexMonthToNum m = fromJust . lookup m $ monthNums
    monthNums = zip ["jan", "feb", "mar", "apr", "may", "jun",
                     "jul", "aug", "sep", "oct", "nov", "dec"] (1..)
        
        
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
