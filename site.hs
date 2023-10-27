{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           BibTeX                        (BibEntry (..), BibTeX,
                                                readBibFile)
import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerThrow, compilerTry)

import           Control.Monad                 (forM, liftM)
import           Data.List                     (intercalate, sortBy)
import           Data.List.Split               (splitOn)
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Ord                      (comparing)
import           Data.String.Utils             (strip)
import           Data.Time                     (UTCTime (..), defaultTimeLocale,
                                                fromGregorian)
import Text.Read (Lexeme(String))

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = readBibFile "data/publications.bib" >>= runHakyll

runHakyll :: BibTeX -> IO ()
runHakyll bibEntries = hakyllWith config $ do

    match "CNAME" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route idRoute
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
        items <- take 15 <$> recentFirst' (posts <> pubs)

        let sidebarField = listField "sidebarItems" jointPubPostContext (return items)
        makeItem "" >>= loadAndApplyTemplate "templates/sidebar.html" sidebarField

    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    -- dummy pages to have raw layouted blog posts for loading.
    match "posts/*" $ version "raw" $
        compile $ do
            pandocCompiler >>= saveSnapshot "content" >>=
                loadAndApplyTemplate "templates/post.html" postCtx

    -- finalising posts including navigation and sidebar
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" sidebarCtx
                >>= relativizeUrls

    -- Dummy entities to save bibEntries
    create [fromFilePath . ("pubs/"++) . bibEntryId $ b | b <- bibEntries] $ version "raw" $ do
        compile $ do
            id_ <- getUnderlying
            let citekey = drop 5 . toFilePath $ id_
            let [bibEntry] = filter ((== citekey) . bibEntryId) bibEntries
            makeItem (bibEntryType bibEntry, bibEntryId bibEntry, bibEntryFields bibEntry) >>= saveSnapshot "bibEntry"
            makeItem ("" :: String)

    -- creating publication pages including navigation and sidebar
    create [fromFilePath . ("pubs/"++) . bibEntryId $ b | b <- bibEntries] $ do
        route $ setExtension "html"
        compile $ do
            sidebarCtx <- loadSidebarContext            
            makeItem ""
                >>= loadAndApplyTemplate "templates/publication.html" pubCtx
                >>= loadAndApplyTemplate "templates/base.html" (boolField "showNoTitle" (const True) <> sidebarCtx)
                >>= relativizeUrls

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            allPubs <- recentFirst' =<< loadAll ("pubs/*" .&&. hasVersion "raw")
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
    field "published" (fmap makeBibTexDateField . getBibEntry) <>
    makeSourceField "source" <>
    makeAuthorField <>
    makeImageField <>
    makePDFfield <>
    makeUrlField <>
    makeKeywordFields <>
    makeBibFields
  where
    makeBibFields :: Context String
    makeBibFields = Context $ \k _ i -> do
        BibEntry _ citekey bibFields <- getBibEntry i
        case lookup k bibFields of
            Just res -> return $ StringField res
            Nothing -> noResult $ "bibEntry for " ++ citekey ++ " does not have field " ++ k
    makeKeywordFields :: Context String
    makeKeywordFields = Context $ \k _ i -> do
        BibEntry _ citekey bibFields <- getBibEntry i
        let keywordsStr = fromMaybe "" (lookup "keywords" bibFields)
        let keywords = map strip . splitOn ";" $ keywordsStr
        if k `elem` keywords then
            return EmptyField
        else
            noResult $ "keyword " ++ k ++ " not present in keywords " ++ keywordsStr
    makeImageField :: Context String
    makeImageField = field "image" (\item -> do
        citekey <- bibEntryId <$> getBibEntry item
        imgItem <- compilerTry (getImage citekey)
        case imgItem of
            Left _ -> noResult $ "no image for " ++ citekey
            Right i -> do
                mr <- getRoute . itemIdentifier $ i
                return $ fromJust mr)
    makePDFfield :: Context String
    makePDFfield = field "pdf" (\item -> do
        citekey <- bibEntryId <$> getBibEntry item
        pdfItem <- compilerTry (getPDF citekey)
        case pdfItem of
            Left _ -> noResult $ "no PDF for " ++ citekey
            Right i -> do
                mr <- getRoute . itemIdentifier $ i
                return $ fromJust mr)
    makeUrlField :: Context String
    makeUrlField = field "internal_url" (return . (++ ".html") . toFilePath . itemIdentifier)
    getImage :: String -> Compiler (Item CopyFile)
    getImage ck = load . fromFilePath $ "images/publications/" ++ ck ++ ".jpg"
    getPDF :: String -> Compiler (Item CopyFile)
    getPDF ck = load . fromFilePath $ "data/pdfs/" ++ ck ++ ".pdf"
    makeSourceField :: String -> Context String
    makeSourceField key = field key (\item -> do
        BibEntry _ citekey bibFields <- getBibEntry item
        case lookup "journal" bibFields of
            Just j -> return j
            Nothing -> case lookup "booktitle" bibFields of
                Just b -> return b
                Nothing -> noResult $ "bibEntry for " ++ citekey ++
                    " does not have fields journal or booktitle")
    makeAuthorField :: Context String
    makeAuthorField = functionField "authors" (\args item -> do
        authors <- getAuthors item
        case args of
            ["abbrv"] -> case authors of
                [firstAuthor]               -> return $ snd firstAuthor
                [firstAuthor, secondAuthor] -> return $ snd firstAuthor ++ " and " ++ snd secondAuthor
                (firstAuthor : _)           -> return $ snd firstAuthor ++ " et al."
            _ -> case authors of
                [firstAuthor] -> return $ renderAuthor firstAuthor
                [firstAuthor, secondAuthor] -> return $ renderAuthor firstAuthor ++ " and " ++ renderAuthor secondAuthor
                (firstAuthor : _)  ->
                    case args of
                        ["short"] -> return $ renderAuthor firstAuthor ++ " et al."
                        ["full"] -> return $ intercalate ", " [renderAuthor a | a <- init authors] ++ " and " ++
                                        renderAuthor (last authors)
        )
    renderAuthor :: (String, String) -> String
    renderAuthor ("Stephan", "Schiffels") = "<u>Stephan Schiffels</u>"
    renderAuthor (firstName, lastName) = firstName ++ " " ++ lastName

getBibEntry :: Item String -> Compiler BibEntry
getBibEntry item = do
    (bibType, bibKey, bibFields) <- itemBody <$> loadSnapshot (setVersion (Just "raw") $ itemIdentifier item) "bibEntry"
    return $ BibEntry bibType bibKey bibFields

getAuthors :: Item String -> Compiler [(String, String)]
getAuthors item = do
    BibEntry _ citekey bibFields <- getBibEntry item
    case lookup "author" bibFields of
        Just allAuthorsStr -> do
            forM (splitOn " and " (intercalate " " . map strip . lines $ allAuthorsStr)) $ \singleAuthorStr -> do
                case splitOn ", " singleAuthorStr of
                    [lastName, firstName] -> return (firstName, lastName)
                    [firstName] -> return (firstName, "") -- in some cultures there are single first names, e.g. "Nini"
                    _ -> compilerThrow $ ["cannot parse author" ++ singleAuthorStr]
        Nothing -> noResult $ "bibEntry for " ++ citekey ++ " does not have field author"

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
    constField "is_post" "True" <>
    dateField "date" "%B %e, %Y" <>
    teaserField "teaser" "content" <>
    field "content_body" (\item -> itemBody <$> loadSnapshot (itemIdentifier item) "content") <>
    field "url" (fmap fromJust . getRoute . setVersion Nothing . itemIdentifier) <>
    defaultContext
