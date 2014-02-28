{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Main (main) where

import Text.Julius
import Yesod
import Yesod.Static
import Yesod.Angular
import Data.IORef
import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map
import Types

data App = App
    { getStatic :: Static
    , ipeople :: IORef (Map PersonId Person)
    , nextId :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR
/static StaticR Static getStatic
|]

instance Yesod App
instance YesodAngular App where
    urlAngularJs _ = Left $ StaticR $ StaticRoute ["angular", "angular.js"] []
    urlAngularRouteJs _ = Left $ StaticR $ StaticRoute ["angular", "angular-route.js"] []

type Angular = AngularT App ()

handleHomeR :: Handler Html
handleHomeR = runAngular $ do
    cmdGetPeople <- addCommand $ \() -> do
        people' <- getYesod >>= liftIO . readIORef . ipeople
        return $ map (\(pid, Person name _) -> PersonSummary pid name) $ Map.toList people'
    $(addCtrl "/people" "people")

    cmdPersonDetail <- addCommand $ \(Singleton pid) -> do
        app <- getYesod
        m <- liftIO $ readIORef $ ipeople app
        case Map.lookup pid m of
            Nothing -> notFound
            Just p -> return p
    $(addCtrl "/people/:personId" "person-detail")

    cmdAddPerson <- addCommand $ \p -> do
        app <- getYesod
        i <- fmap (PersonId . pack . show) $ liftIO $ atomicModifyIORef (nextId app) $ \i -> (i + 1, i + 1)
        () <- liftIO $ atomicModifyIORef (ipeople app) $ \m ->
            (Map.insert i p m, ())
        return $ Singleton i
    $(addCtrl "/add-person" "add-person")

    setDefaultRoute "/people"

main :: IO ()
main = do
    s <- static "static"
    p <- newIORef Map.empty
    ni <- newIORef 1
    warp 3000 $ App s p ni
