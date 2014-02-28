{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yesod.Angular
    ( YesodAngular (..)
    , runAngular
    , addCommand
    , addCtrl
    , addCtrlRaw
    , setDefaultRoute
    , AngularT
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First (..), Monoid (..))
import           Data.Text                  (Text)
import           Yesod
import           Text.Hamlet
import           Text.Julius
import Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE), Lit (StringL))
import qualified Data.Text as T
import Data.Char (isAlpha)

class Yesod master => YesodAngular master where
    urlAngularJs :: master -> Either (Route master) Text
    urlAngularJs _ = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.12/angular.js"
    urlAngularRouteJs :: master -> Either (Route master) Text
    urlAngularRouteJs _ = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.2.12/angular-route.js"

    wrapAngular :: Text -> WidgetT master IO () -> HandlerT master IO Html
    wrapAngular modname widget = defaultLayout [whamlet|<div ng-app=#{modname}>^{widget}|]

data AngularWriter master = AngularWriter
    { awCommands     :: Map Text (HandlerT master IO ())
    , awPartials     :: Map Text (HtmlUrl (Route master))
    , awRoutes       :: JavascriptUrl (Route master)
    , awControllers  :: JavascriptUrl (Route master)
    , awDefaultRoute :: First Text
    }
instance Monoid (AngularWriter master) where
    mempty = AngularWriter mempty mempty mempty mempty mempty
    AngularWriter a1 a2 a3 a4 a5
        `mappend` AngularWriter b1 b2 b3 b4 b5
        = AngularWriter
            (mappend a1 b1)
            (mappend a2 b2)
            (mappend a3 b3)
            (mappend a4 b4)
            (mappend a5 b5)

type AngularT master = WriterT (AngularWriter master) (HandlerT master IO)

runAngular :: YesodAngular master
           => AngularT master () -> HandlerT master IO Html
runAngular ga = do
    master <- getYesod
    ((), AngularWriter{..}) <- runWriterT ga
    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just htmlurl -> getUrlRenderParams >>= sendResponse . htmlurl

    modname <- newIdent

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:"#{rawJS x}"})|]
                First Nothing -> mempty

    wrapAngular modname $ do
        addScriptEither $ urlAngularJs master
        addScriptEither $ urlAngularRouteJs master
        [whamlet|<div ng-view>|]
        toWidget [julius|
'use static';
angular
    .module("#{rawJS modname}", ['ngRoute'])
    .config(["$routeProvider", function ($routeProvider) {
        $routeProvider^{awRoutes}^{defaultRoute};
    }]);
    ^{awControllers}
|]

addCommand :: (FromJSON input, ToJSON output)
           => (input -> HandlerT master IO output)
           -> AngularT master Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" `mappend` name
  where
    handler = do
        input <- parseJsonBody_
        output <- f input
        repjson <- returnJson output
        sendResponse repjson

addCtrl :: Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl route name = do
    let name' = T.filter isAlpha name
    [|addCtrlRaw $(liftT name') $(liftT route) $(hamletFile $ fn "hamlet") $(juliusFile $ fn "julius")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/", name, ".", suffix]

addCtrlRaw :: Text -- ^ user-friendly name
           -> Text -- ^ route pattern
           -> HtmlUrl (Route master) -- ^ template
           -> JavascriptUrl (Route master) -- ^ controller
           -> AngularT master ()
addCtrlRaw name' route template controller = do
    name <- (mappend $ mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes = [julius|.when("#{rawJS route}", {controller: #{rawJS name}, templateUrl: "?partial=#{rawJS name}"})|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        }

setDefaultRoute :: Text -> AngularT master ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }
