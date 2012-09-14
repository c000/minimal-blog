{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs #-}

import Data.Text
import Yesod
import Database.Persist.Sqlite

data MinimalBlog = MinimalBlog Connection

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Entry
    entryTitle Text
    entryContnt Text
    deriving Show
|]

mkYesod "MinimalBlog" [parseRoutes|
/ HomeR GET
|]

instance Yesod MinimalBlog
instance YesodPersist MinimalBlog where
    type YesodPersistBackend MinimalBlog = SqlPersist
    runDB action = do
        MinimalBlog conn <- getYesod
        runSqlConn action conn

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Minimal Blog!"
    [whamlet|
<h1>Minimal Blog
<h2>Entries
HelloWorld!
|]

main :: IO ()
main = withSqliteConn ":memory:" $ \sqlConn -> do
    runSqlConn (runMigration migrateAll) sqlConn
    warpDebug 3000 $ MinimalBlog sqlConn
