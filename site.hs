{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           BibTeX                 (BibEntry (..), BibTeX, readBibFile)
import           Hakyll

import           Control.Monad          (liftM, void)
import           Data.List              (sortBy)
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)
import           Data.Time              (UTCTime (..), defaultTimeLocale, fromGregorian)
import           System.FilePath.Posix  (replaceExtension, takeFileName)
import Text.Read (Lexeme(String))

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
        posts <- loadAll ("posts/*" .&&. hasVersion "raw")
        pubs <- loadAll ("pubs/*" .&&. hasVersion "raw")
        items <- recentFirst' (posts <> pubs)

        let sidebarField = listField "sidebarItems" jointPubPostContext (return items)
        makeItem "" >>= loadAndApplyTemplate "templates/sidebar.html" sidebarField

    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls
    
    -- preparing posts pages
    match "posts/*" $ version "raw" $
        compile $ do
            pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx
 
    -- finalising posts including navigation and sidebar
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            id_ <- getUnderlying
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    -- preparing publication pages
    create [fromFilePath . ("pubs/"++) . bibEntryId $ b | b <- bibEntries] $ version "raw" $ do
        compile $ do
            id_ <- getUnderlying
            let citekey = drop 5 . toFilePath $ id_
            let [bibEntry] = filter ((== citekey) . bibEntryId) bibEntries
            makeItem (bibEntryType bibEntry, bibEntryId bibEntry, bibEntryFields bibEntry) >>= saveSnapshot "bibEntry"
            makeItem ("" :: String)

    -- finalising publication pages including navigation and sidebar
    create [fromFilePath . ("pubs/"++) . bibEntryId $ b | b <- bibEntries] $ do
        route $ setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/publication.html" pubCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            allPubs <- loadAll ("pubs/*" .&&. hasVersion "raw")
            sidebarCtx <- loadSidebarContext
            let ctx = listField "publications" pubCtx (return allPubs)
            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" (constField "title" "Publications" <> constField "menu_Publications" "True" <> sidebarCtx)
                >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
            sidebarCtx <- loadSidebarContext
            let ctx = listField "posts" postCtx (return posts)
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" (constField "title" "Blog" <> constField "menu_Blog" "True" <> sidebarCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

recentFirst' :: [Item String] -> Compiler [Item String]
recentFirst' = liftM reverse . chronological'
  where
    chronological' :: [Item String] -> Compiler [Item String]
    chronological' = sortByM $ \i -> do
        if (take 5 . toFilePath . itemIdentifier $ i) == "pubs/" then
            makeBibTexDate <$> getBibEntry i
        else
            getItemUTC defaultTimeLocale . itemIdentifier $ i
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

jointPubPostContext :: Context String
jointPubPostContext = Context $ \k a i -> do
    let Context f = pubCtx
        Context g = postCtx
    if (take 5 . toFilePath . itemIdentifier $ i) == "pubs/" then f k a i else g k a i

loadSidebarContext :: Compiler (Context String)
loadSidebarContext = do
    sidebar <- itemBody <$> load "sidebar"
    return $ constField "sidebar" sidebar <> defaultContext

pubCtx :: Context String
pubCtx =
    constField "is_pub" "True" <>
    makeBibField "title" <>
    makeSourceField "source" <>
    field "published" (fmap makeBibTexDateField . getBibEntry) <>
    makeBibField "author" <>
    field "citekey" (fmap bibEntryId . getBibEntry) <>
    makeImageField <>
    makeBibField "url" <>
    makeBibField "abstract"
  where
    makeBibField :: String -> Context String
    makeBibField key = field key (\item -> do
        BibEntry _ citekey bibFields <- getBibEntry item
        case lookup key bibFields of
            Just res -> return res
            Nothing -> noResult $ "bibEntry for " ++ citekey ++ " does not have field " ++ key)
    makeImageField :: Context String
    makeImageField = field "image" (\item -> do
        citekey <- bibEntryId <$> getBibEntry item
        imgItem <- getImage citekey
        fmap fromJust . getRoute . itemIdentifier $ imgItem)
    getImage :: String -> Compiler (Item String)
    getImage ck = load . fromFilePath $ "images/publications/" ++ ck ++ ".jpg"
    makeSourceField :: String -> Context String
    makeSourceField key = field key (\item -> do
        BibEntry _ citekey bibFields <- getBibEntry item
        case lookup "journal" bibFields of
            Just j -> return j
            Nothing -> case lookup "booktitle" bibFields of
                Just b -> return b
                Nothing -> noResult $ "bibEntry for " ++ citekey ++
                    " does not have fields journal or booktitle")

getBibEntry :: Item String -> Compiler BibEntry
getBibEntry item = do
    (bibType, bibKey, bibFields) <- itemBody <$> loadSnapshot (setVersion (Just "raw") $ itemIdentifier item) "bibEntry"
    return $ BibEntry bibType bibKey bibFields

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
    boolField "is_post" isPost <>
    dateField "date" "%B %e, %Y" <>
    field "url" (fmap fromJust . getRoute . setVersion Nothing . itemIdentifier) <>
    defaultContext
  where
    isPost = (=="posts/") . take 6 . toFilePath . itemIdentifier
