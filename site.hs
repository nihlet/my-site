--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc.Options
import           Lab2
import           Lab3
import           Lab4
import           Lab5
--------------------------------------------------------------------------------
main :: IO ()
main = do  
    -- lab2
    -- lab3
    -- lab4
    lab5
    hakyll $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "posts/**.svg" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
            
        match "third-party/**" $ do
            route   idRoute
            compile copyFileCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/**.md" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/**.md"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/**.md"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%0Y-%m-%d" `mappend`
    defaultContext
