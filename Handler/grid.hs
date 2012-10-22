{-# LANGUAGE OverloadedStrings #-}
module Handler.Grid
    where

import Import
import qualified Data.Text as T

getGridR :: Handler RepHtml
getGridR = defaultLayout $ do

{-
-- this needs to be in head, above the CDN call in order to load local dojo modules, but no jsLoaders work that way
-- http://hackage.haskell.org/packages/archive/yesod-core/latest/doc/html/Yesod-Core.html#v:jsLoader
-- dojo recommends loading scripts in body to imrpove page loading time, but i don't think possible with addScript*

    toWidgetHead [julius|
var dojoConfig = {
    async: true,
    // This code registers the correct location of the
    // package so we can load Dojo from the CDN whilst still
    // being able to load local modules
    packages: [{
        name: "dgrid",
        location: location.pathname.replace(/\/[^/]+$/, '')
    }]
};
|]

    addScriptRemoteAttrs "//ajax.googleapis.com/ajax/libs/dojo/1.8.0/dojo/dojo.js" [] -- 1.8.1 is 404'ing
-}

{-
    let c = T.unlines
              [ " async: 1,                                                                                       " 
              , " dojoBlankHtmlUrl: '/../static/cpm_packages/dojo/resources/blank.html',                          "
              , " packages: [ {                                                                                   "
              , "   name: 'dgrid',                                                                                "
              , "   location: location.pathname.replace(/\\/[^/]+$/, '') + '/../static/cpm_packages/dgrid'        "
              , "  },{                                                                                            "
              , "   name: 'put-selector',                                                                         "
              , "   location: location.pathname.replace(/\\/[^/]+$/, '') + '/../static/cpm_packages/put-selector' "
              , "  },{                                                                                            "
              , "   name: 'xstyle',                                                                               "
              , "   location: location.pathname.replace(/\\/[^/]+$/, '') + '/../static/cpm_packages/xstyle'       "
              , " } ]                                                                                             "
              ]

        {-
        -- multiline string literal not working on windows (\r problem?)
            let c = "async: 1,                                                                                /
                   / packages: [ {                                                                            /
                   /   name: 'dgrid',                                                                         /
                   /   location: location.pathname.replace(/\\/[^/]+$/, '') + '/../static/cpm_packages/dgrid' /
                   / } ]"
        -}

    addScriptRemoteAttrs "//ajax.googleapis.com/ajax/libs/dojo/1.8.0/dojo/dojo.js" [("data-dojo-config",c)] -- 1.8.1 is 404'ing
-}

    let dojoA = [("data-dojo-config","async: true")]

    -- addScriptAttrs (StaticR $ StaticRoute ["cpm_packages/dojo/dojo.js"] []) dojoA --  encodes '/' as '%2f' and we get 403 forbidden
    -- equiv w/compile-time existence check, but have to cabal clean to pick this up
    addScriptAttrs (StaticR cpm_packages_dojo_dojo_js) dojoA -- if use cpm_packages/dojo.js instead of cpm_packages/dojo/dojo.js, get "define undefined"

    --http://hackage.haskell.org/packages/archive/yesod-static/1.0.0.3/doc/html/Yesod-Static.html#v:staticFiles
    --doesn't mention spaces also converted, but seem to be
    --also, how would we have multiple roots for the static subsite (lots of shared js frameworks across sites)?

    domId1 <- lift newIdent
    domId2 <- lift newIdent

    toWidget [julius|
require(["dojo/_base/declare", "dgrid/Grid", "dgrid/Keyboard", "dgrid/Selection", "dojo/dom", "dojo/fx", "dojo/request", "dojo/domReady!"],
    function(declare, Grid, Keyboard, Selection, dom, fx, request){
        var data = [
            { first: "Bob", last: "Barker", age: 89 },
            { first: "Vanna", last: "White", age: 55 },
            { first: "Pat", last: "Sajak", age: 65 }
        ];
 
        // Create a new constructor by mixing in the components
        var CustomGrid = declare([ Grid, Keyboard, Selection ]);
 
        // Now, create an instance of our custom grid which
        // have the features we added!
        var grid = new CustomGrid({
            columns: {
                first: "First Name",
                last: "Last Name",
                age: "Age"
            },
            selectionMode: "single", // for Selection; only select a single row at a time
            cellNavigation: false // for Keyboard; allow only row-level keyboard navigation
        }, #{domId2});

        grid.renderArray(data);

        grid.on(".dgrid-row:click", function(event){
            console.log("Row clicked:", grid.row(event).id);
 
            dom.byId(#{domId1}).innerHTML += event;

            fx.slideTo({
                top: 100,
                left: 200,
                node: #{domId1}
              }).play();

            request("static/helloworld.txt").then(
                function(text){
                    console.log("The file's contents is: " + text);
                },
                function(error){
                    console.log("An error occurred: " + error);
                }
            );

        });
});
|]

    [whamlet| 
<h1 id=#{domId1}>Hello
<div id=#{domId2}>
|]