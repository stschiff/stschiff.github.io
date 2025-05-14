{-# LANGUAGE OverloadedStrings #-}

import           BibTeX                        (BibEntry (..), bibFileParser)
import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerThrow, compilerTry)


import           Control.Monad                 (forM)
import Text.Parsec (runParser)
import           Data.List                     (intercalate, sortOn, elemIndex)
import           Data.List.Split               (splitOn)
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Ord                      (Down(..))
import           Data.String.Utils             (strip)
import           Data.Time                     (UTCTime (..), fromGregorian)

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do

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

    match "pdfs/*" $ do
        route idRoute
        compile copyFileCompiler

    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/base.html" defaultContext
                >>= relativizeUrls

    -- dummy pages to have raw layouted blog posts for loading.
    match "posts/*" $ version "raw" $
        compile $ do
            pandocCompiler >>= saveSnapshot "content" >>=
                loadAndApplyTemplate "templates/post.html" postCtx

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" defaultContext
                >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
            let ctx = listField "posts" postCtx (return posts)
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" (constField "title" "Blog" <> constField "menu_Blog" "True" <> defaultContext)
                >>= relativizeUrls

    match "pages/publications.bib" $ do
        route (constRoute "publications.html")
        compile $ do
            bibRaw <- itemBody <$> getResourceString
            let res = runParser bibFileParser () "" bibRaw
            let bibEntries = case res of
                    Left err   -> error (show err)
                    Right res_ -> res_
            let sortedBibEntries = sortOn (Down . makeBibTexDate) bibEntries
            makeItem "" >>= loadAndApplyTemplate "templates/publications.html" (pubListCtx sortedBibEntries)
                        >>= loadAndApplyTemplate "templates/base.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

pubListCtx :: [BibEntry] -> Context String
pubListCtx bibEntries = listField "publications" pubCtx (mapM makeItem bibEntries) 

pubCtx :: Context BibEntry
pubCtx =
    constField "is_pub" "True" <>
    field "published" (return . makeBibTexDateField . itemBody) <>
    makeSourceField "source" <>
    makeAuthorField <>
    makeImageField <>
    makePDFfield <>
    makeKeywordFields <>
    makeBibFields
  where
    makeBibFields :: Context BibEntry
    makeBibFields = Context $ \k _ i -> do
        let BibEntry _ citekey bibFields = itemBody i
        case lookup k bibFields of
            Just res -> return $ StringField res
            Nothing -> noResult $ "bibEntry for " ++ citekey ++ " does not have field " ++ k
    makeKeywordFields :: Context BibEntry
    makeKeywordFields = Context $ \k _ i -> do
        let BibEntry _ _ bibFields = itemBody i
        let keywordsStr = fromMaybe "" (lookup "keywords" bibFields)
        let keywords = map strip . splitOn ";" $ keywordsStr
        if k `elem` keywords then
            return EmptyField
        else
            noResult $ "keyword " ++ k ++ " not present in keywords " ++ keywordsStr
    makeImageField :: Context BibEntry
    makeImageField = field "image" (\item -> do
        let citekey = bibEntryId . itemBody $ item
        imgItem <- compilerTry (getImage citekey)
        case imgItem of
            Left _ -> noResult $ "no image for " ++ citekey
            Right i -> do
                mr <- getRoute . itemIdentifier $ i
                return $ fromJust mr)
    makePDFfield :: Context BibEntry
    makePDFfield = field "pdf" (\item -> do
        case lookup "pdf_url" . bibEntryFields . itemBody $ item of
            Just url -> return url
            Nothing -> do
                let citekey = bibEntryId . itemBody $ item
                pdfItem <- compilerTry (getPDF citekey)
                case pdfItem of
                    Left _ -> noResult $ "no PDF for " ++ citekey
                    Right i -> do
                        mr <- getRoute . itemIdentifier $ i
                        return $ fromJust mr)
    getImage :: String -> Compiler (Item CopyFile)
    getImage ck = load . fromFilePath $ "images/publications/" ++ ck ++ ".jpg"
    getPDF :: String -> Compiler (Item CopyFile)
    getPDF ck = load . fromFilePath $ "pdfs/" ++ ck ++ ".pdf"
    makeSourceField :: String -> Context BibEntry
    makeSourceField key = field key (\item -> do
        let BibEntry _ citekey bibFields = itemBody item
        case lookup "journal" bibFields of
            Just j -> return j
            Nothing -> case lookup "booktitle" bibFields of
                Just b -> return b
                Nothing -> noResult $ "bibEntry for " ++ citekey ++
                    " does not have fields journal or booktitle")
    makeAuthorField :: Context BibEntry
    makeAuthorField = functionField "authors" (\args item -> do
        authors <- getAuthors item
        case args of
            ["abbrv"] -> case authors of
                [firstAuthor]               -> return $ snd firstAuthor
                [firstAuthor, secondAuthor] -> return $ snd firstAuthor ++ " and " ++ snd secondAuthor
                (firstAuthor : _)           -> return $ snd firstAuthor ++ " et al."
                _ -> error "should never happen"
            _ -> case authors of
                [firstAuthor] -> return $ renderAuthor firstAuthor
                [firstAuthor, secondAuthor] -> return $ renderAuthor firstAuthor ++ " and " ++ renderAuthor secondAuthor
                (firstAuthor' : _)  ->
                    case args of
                        ["short"] -> return $ renderAuthor firstAuthor' ++ " et al."
                        ["condensed"] ->
                            case splitOnMe authors of
                                ([], me, []) -> return $ renderAuthor me
                                ([firstAuthor], me, []) -> return $ renderAuthor firstAuthor ++ " and " ++ renderAuthor me
                                (firstAuthor : _, me, []) -> return $ renderAuthor firstAuthor ++ " ... and " ++ renderAuthor me
                                ([], me, [lastAuthor]) -> return $ renderAuthor me ++ " and " ++ renderAuthor lastAuthor
                                ([], me, rest) -> return $ renderAuthor me ++ " ... and " ++ renderAuthor (last rest)
                                ([firstAuthor], me, [lastAuthor]) -> return $ renderAuthor firstAuthor ++ ", " ++ renderAuthor me ++ " and " ++ renderAuthor lastAuthor
                                (firstAuthor : _, me, [lastAuthor]) -> return $ renderAuthor firstAuthor ++ ", ... " ++ renderAuthor me ++ " and " ++ renderAuthor lastAuthor
                                ([firstAuthor], me, rest) -> return $ renderAuthor firstAuthor ++ ", " ++ renderAuthor me ++ " ... and " ++ renderAuthor (last rest)
                                (firstAuthor : _, me, rest) -> return $ renderAuthor firstAuthor ++ ", ... " ++ renderAuthor me ++ " ... and " ++ renderAuthor (last rest)
                        ["full"] -> return $ intercalate ", " [renderAuthor a | a <- init authors] ++ " and " ++
                                        renderAuthor (last authors)
                        _ -> error "should never happen"
                _ -> error "should never happen"

        )
    renderAuthor :: (String, String) -> String
    renderAuthor ("Stephan", "Schiffels") = "<u>Stephan Schiffels</u>"
    renderAuthor (firstName, lastName) = firstName ++ " " ++ lastName
    splitOnMe :: [(String, String)] -> ([(String, String)], (String, String), [(String, String)])
    splitOnMe authors = 
        let meIndex = fromJust $ elemIndex ("Stephan", "Schiffels") authors
        in  (take meIndex authors, authors !! meIndex, drop (meIndex + 1) authors)

getAuthors :: Item BibEntry -> Compiler [(String, String)]
getAuthors item = do
    let BibEntry _ citekey bibFields = itemBody item
    case lookup "author" bibFields of
        Just allAuthorsStr -> do
            forM (splitOn " and " (unwords . map strip . lines $ allAuthorsStr)) $ \singleAuthorStr -> do
                case splitOn ", " singleAuthorStr of
                    [lastName, firstName] -> return (firstName, lastName)
                    [firstName] -> return (firstName, "") -- in some cultures there are single first names, e.g. "Nini"
                    _ -> compilerThrow ["cannot parse author" ++ singleAuthorStr]
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
            _ ->
                case sequence (take 2 maybeDateFields) of
                    Just [year, month] -> bibTexMonthToStr month ++ ", " ++ year
                    _ -> error $ "could not find date information in BibEntry " ++ bibEntryId bibEntry
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
