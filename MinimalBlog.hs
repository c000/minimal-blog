{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs #-}

import Control.Applicative
import Data.Text
import Yesod
import Database.Persist.Sqlite

data MinimalBlog = MinimalBlog Connection

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Entry
    title Text
    content Textarea
    deriving Show
|]

mkYesod "MinimalBlog" [parseRoutes|
/ HomeR GET POST
/entry/#EntryId DetailR GET
|]

instance Yesod MinimalBlog
instance RenderMessage MinimalBlog FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodPersist MinimalBlog where
    type YesodPersistBackend MinimalBlog = SqlPersist
    runDB action = do
        MinimalBlog conn <- getYesod
        runSqlConn action conn

entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing

getHomeR :: Handler RepHtml
getHomeR = do
    entries <- runDB $ selectList [] []
    (form, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "Minimal Blog!"
        liftIO $ print (entries :: [Entity Entry])
        [whamlet|
            <h1>Minimal Blog
            <h2>Entries
            <ul>
                $forall Entity entryId entry <- entries
                    <li>
                        <a href=@{DetailR entryId}>#{entryTitle entry}
            <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{form}
                <input type=submit>
        |]

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, form), enctype) <- runFormPost entryForm
    case result of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            redirect $ DetailR entryId
        _ -> do
            redirect $ HomeR

getDetailR :: EntryId -> Handler RepHtml
getDetailR entryId = do
    entry <- runDB $ get404 entryId
    defaultLayout $ do
        [whamlet|
<h1>Minimal Blog - Entry Detail
<h2>#{entryTitle entry}
<p>
    #{entryContent entry}
|]

main :: IO ()
main = withSqliteConn ":memory:" $ \sqlConn -> do
    runSqlConn (runMigration migrateAll) sqlConn
    runSqlConn (insert $ Entry "SampleEntry" (Textarea "Contents")) sqlConn
    warpDebug 3000 $ MinimalBlog sqlConn
