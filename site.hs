{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, defaultTimeLocale, getCurrentTime, addUTCTime, nominalDay)
import System.FilePath.Posix (takeFileName, replaceExtension)
import Text.Pandoc.Definition (Pandoc(..), lookupMeta)
import Text.Pandoc.Readers.BibTeX (readBibTeX)
import Text.Pandoc.Options (def)
import Text.Pandoc.Class (runIO)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
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
            baseCtx <- getBaseCtx Nothing
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

    match "data/publications.bib" $ do
        compile $ do
            Biblio referenceBS <- itemBody <$> biblioCompiler
            Right (Pandoc meta _) <- unsafeCompiler . runIO $ readBibTeX def referenceBS
            let Just references = lookupMeta "references" meta
            unsafeCompiler (print references)
            pandocCompiler
            -- return ""
            -- refItems <- mapM makeItem references
            -- let pubCtx =
            --         field "title" (show . title . itemBody) <>
            --         field "journal" (show . source . itemBody)
            --     ctx = listField "publications" pubCtx (return refItems)
            -- makeItem ""
            --     >>= loadAndApplyTemplate "templates/publications.html" ctx
            --     >>= loadAndApplyTemplate "templates/base.html"
        
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
            Just t -> constField "title" t
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
        Just _ -> return True
        Nothing -> return False
