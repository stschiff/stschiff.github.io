{-# LANGUAGE OverloadedStrings #-}
import           BibTeX                 (bibFileParser, BibEntry(..))
import           Data.Monoid            (mappend)
import           Hakyll

import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (UTCTime (..), addUTCTime,
                                         defaultTimeLocale, fromGregorian,
                                         getCurrentTime, nominalDay,
                                         secondsToDiffTime)
import           Debug.Trace            (trace)
import           System.FilePath.Posix  (replaceExtension, takeFileName)
import           Text.Parsec            (parse)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "images/publications/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            baseCtx <- getBaseCtx Nothing
            ext <- getUnderlyingExtension
            let c = if ext == ".html" then getResourceBody else pandocCompiler
            c
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "group_members/*" $ do
        compile pandocCompiler

    create ["group.html"] $ do
        route idRoute
        compile $ do
            let groupCtx = listField "members" defaultContext (loadAll "group_members/*")
            baseCtx <- getBaseCtx (Just "Group")
            makeItem "MemberPage" >>=
                loadAndApplyTemplate "templates/group.html" groupCtx >>=
                loadAndApplyTemplate "templates/base.html" baseCtx >>=
                relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            baseCtx <- getBaseCtx Nothing
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "posts/*" $ version "raw" $ do
        compile getResourceString

    match "data/publications_short.bib" $ do
        route $ constRoute "publications.html"
        compile $ do
            bibFileStr <- itemBody <$> getResourceString
            let bibEntries = case parse bibFileParser "" bibFileStr of
                    Left err -> error $ "cannot parse Bib file: " ++ show err
                    Right b  -> b
            refItems <- mapM makeItem bibEntries
            baseCtx <- getBaseCtx (Just "Publications")
            let pubCtx =
                    field "title" (return . fromMaybe "" . lookup "title" . bibEntryFields . itemBody) <>
                    field "journal" (return . fromMaybe "" . lookup "journal" . bibEntryFields . itemBody) <>
                    field "citekey" (return . bibEntryId . itemBody)
                ctx = listField "publications" pubCtx (return refItems)
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

getBaseCtx :: Maybe String -> Compiler (Context String)
getBaseCtx maybeTitle = do
    currentTime <- unsafeCompiler getCurrentTime
    let cutoffTime = addUTCTime ((-365) * nominalDay) currentTime
    -- let cutoffTime = UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0)
    posts <- loadAll ("posts/*" .&&. hasVersion "raw")
    pubs <- loadAll "publications/*"
    let filteredPosts = posts -- filterM (isMoreRecentThan cutoffTime) posts
    let filteredPubs = pubs -- filterM (isMoreRecentThan cutoffTime) pubs
    filteredSelectedPubs <- filterM isSelected filteredPubs
    let sidebarField = listField "sidebarItems" postCtx (fmap (take 5) . recentFirst $ filteredPosts <> filteredPubs)
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
