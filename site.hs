--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["pages/contact.md", "pages/people.md", "pages/resources.md"]) $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "publications/*" $ do
        route $ setExtension "html"
        compile pandocCompiler
            -- >>= loadAndApplyTemplate "templates/publication.html"    postCtx
            -- >>= loadAndApplyTemplate "templates/default.html" postCtx
            -- >>= relativizeUrls
        
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    create ["publications.html"] $ do
        route idRoute
        compile $ do
            pubs <- recentFirst =<< loadAll "publications/*"
            let pubCtx =
                    listField "pubs" postCtx (return pubs) `mappend`
                    constField "title" "Publications" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/publications.html" pubCtx
                >>= loadAndApplyTemplate "templates/default.html" pubCtx
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            pubs <- recentFirst =<< loadAll "publications/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "pubs" postCtx (return pubs)   `mappend`
                    constField "title" "Home"                `mappend`
                    constField "isHome" "True"               `mappend`
                    defaultContext

            pandocCompiler    
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
