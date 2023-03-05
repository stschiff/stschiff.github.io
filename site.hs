--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, defaultTimeLocale, getCurrentTime, addUTCTime, nominalDay)
import System.FilePath.Posix (takeFileName, replaceExtension)

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
    
    match "posts/*" $ version "raw" $ do
        compile getResourceString


    match "publications/*" $ do
        compile pandocCompiler
        
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let blogCtx =
                    listField  "posts" postCtx (return posts) <>
                    defaultContext
            baseCtx <- getBaseCtx (Just "Blog")
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/base.html" (baseCtx <> constField "menu_Blog" "True")
                >>= relativizeUrls
                    
    createPublications False "publications.html"
    createPublications True "publications-selected.html"

    match "templates/*" $ compile templateBodyCompiler

createPublications :: Bool -> Identifier -> Rules ()
createPublications selected target = do
    let (f, title, linkTarget, linkName) =
            if selected
            then (isSelected, "Selected Publications", "/publications.html", "Show all")
            else (const (return True), "Publications", "/publications-selected.html", "Show selected only")
    create [target] $ do
        route idRoute
        compile $ do
            pubs <- recentFirst =<< loadAll "publications/*"
            filteredPubs <- filterM f pubs
            let pubCtx =
                    listField  "pubs"  postCtx (return filteredPubs) <>
                    defaultContext
            baseCtx <- getBaseCtx (Just title)
            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" (pubCtx <>
                        constField "linkName" linkName <>
                        constField "linkTarget" linkTarget)
                >>= loadAndApplyTemplate "templates/base.html" (baseCtx <>
                                                                constField "menu_Publications" "True")
                >>= relativizeUrls


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
