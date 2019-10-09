--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, defaultTimeLocale, getCurrentTime, addUTCTime, nominalDay)
import System.FilePath.Posix (takeFileName)

--------------------------------------------------------------------------------
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
            baseCtx <- getBaseCtx True True Nothing
            pandocCompiler
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            baseCtx <- getBaseCtx False True Nothing
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "publications/*" $ do
        compile pandocCompiler
        
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField  "posts" postCtx (return posts) <>
                    defaultContext
            baseCtx <- getBaseCtx False True (Just "Blog")
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/base.html" (baseCtx <> constField "menu_Blog" "True")
                >>= relativizeUrls
    
    createPublications "publications.html" "templates/publications.html" "All Publications"
    createPublications "publications-selected.html" "templates/publications-selected.html" "Selected Publications"

    match "templates/*" $ compile templateBodyCompiler

createPublications :: Identifier -> Identifier -> String -> Rules ()
createPublications target template title = do
    create [target] $ do
        route idRoute
        compile $ do
            pubs <- recentFirst =<< loadAll "publications/*"
            let pubCtx =
                    listField  "pubs"  postCtx (return pubs) <>
                    defaultContext
            baseCtx <- getBaseCtx True False (Just title)
            makeItem ""
                >>= loadAndApplyTemplate template pubCtx
                >>= loadAndApplyTemplate "templates/base.html" (baseCtx <> constField "menu_Publications" "True")
                >>= relativizeUrls


postCtx :: Context String
postCtx =
    boolField "pub" isPub <>
    boolField "post" isPost <>
    field "itemName" itemName <>
    dateField "date" "%B %e, %Y" <>
    defaultContext
  where
    isPost = (=="posts/") . take 6 . toFilePath . itemIdentifier
    isPub = (=="publications/") . take 13 . toFilePath . itemIdentifier
    itemName = return . takeFileName . toFilePath . itemIdentifier

getBaseCtx :: Bool -> Bool -> Maybe String -> Compiler (Context String)
getBaseCtx withPosts withPubs maybeTitle = do
    currentTime <- unsafeCompiler getCurrentTime
    let cutoffTime = addUTCTime ((-365) * nominalDay) currentTime
    -- let cutoffTime = UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0)
    posts <- if withPosts
        then
            loadAll "posts/*"
        else
            return mempty
    pubs <- if withPubs
        then
            loadAll "publications/*"
        else
            return mempty
    filteredPosts <- filterM (isMoreRecentThan cutoffTime) posts
    filteredPubs <- filterM (isMoreRecentThan cutoffTime) pubs
    filteredSelectedPubs <- filterM isSelected filteredPubs
    let sidebarField = listField "sidebarItems" postCtx (recentFirst (filteredPosts <> filteredPubs))
        titleField = case maybeTitle of
            Nothing -> mempty
            Just t -> constField "title" t
    return $ sidebarField <> titleField <> defaultContext
  where
    isMoreRecentThan :: (MonadMetadata m) => UTCTime -> Item a -> m Bool
    isMoreRecentThan cutoffTime i = do
        itemTime <- getItemUTC defaultTimeLocale . itemIdentifier $ i
        return (itemTime > cutoffTime)
    isSelected :: (MonadMetadata m) => Item a -> m Bool
    isSelected i = do
        f <- getMetadataField (itemIdentifier i) "selected"
        case f of
            Just _ -> return True
            Nothing -> return False
