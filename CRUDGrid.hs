{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

-- TODO: get rid of these fromJusts

module CRUDGrid
    ( Grid      (..)
    , GridField (..)
    , Editable  (..)
    , Routes    (..)
    , JSGrid    (..)
    , groupGet
    , formGet
    , formPost
    , formDelete
    , formNewGet
    , formNewPost
    , showFieldByID
    , makeStatic
    ) where

--import Import -- creates cycle with Model.hs that blocks TH, so have to import following manually:
import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Yesod hiding (Route (..))
import Control.Applicative
import Data.Text (Text)
-- but then we can't see data constructor StaticR in Foundation, which imports Model, so we're stuck?
import Foundation

import Yesod.Static -- StaticRoute
import Text.Julius -- RawJS
import Control.Arrow
import Control.Monad
import Data.Maybe
import Prelude (head)
import Data.List.Split
import qualified Data.Text as T

data Routes s p =
    Routes { indR    :: Key p -> Route s
           , groupR  :: Route s
           , newR    :: Route s           -- add Maybe for Createable
           , deleteR :: Key p -> Route s  -- add Maybe for Deletable
           }

data Grid s p c = 
    Grid { title       :: Text
         , opts        :: [SelectOpt p] -- need some way to sort on indirect fields
         , allowDelete :: Bool            -- eliminate this for Maybe deleteR in Routes
         , defaultNew  :: Maybe p
         , routes      :: Routes s p
         , jsgrid      :: Maybe (JSGrid s)
         , getField    :: c -> GridField s p c
         }

data GridField s p c = forall t. (PersistField t) =>
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: Either (t -> String) (t -> (Maybe (Route s), String))
            , editable :: Maybe (Editable s t p)
            }

data Editable s t p =
  Editable { vField   :: forall m. ( RenderMessage (HandlerSite m) FormMessage
--                                   , s ~ HandlerSite m
                                   , MonadHandler m
                                   ) 
                                   => Field m t
           , pField   :: EntityField p t
           , required :: Bool
           , updater  :: t -> p -> p -- required to avoid "Record update for insufficiently polymorphic field"
           }

data JSGrid s = JQGrid ([Route s], [Route s])
              | DGrid (Route s) [(Text, Text)]

groupGet, formNewGet, formNewPost
         :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend s (HandlerT s IO))
            , YesodPersist s
            , Yesod s
            , RenderMessage s FormMessage
            , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
            )
         => Grid s p c
         -> HandlerT s IO Html
groupGet g = gridForm False g $ Right Nothing
formNewGet  = formNewPostGen False
formNewPost = formNewPostGen True

formNewPostGen :: ( Bounded c
                  , Enum c
                  , Eq c
                  , YesodPersist s
                  , Yesod s
                  , RenderMessage s FormMessage
                  , PersistEntity p
                  , PersistQuery (YesodPersistBackend s (HandlerT s IO))
                  , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
                  ) 
               => Bool 
               -> Grid s p c 
               -> HandlerT s IO Html
formNewPostGen r g = gridForm r g $ Left . fromJust $ defaultNew g

formGet, formPost, formDelete
         :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend s (HandlerT s IO))
            , PersistStore (YesodPersistBackend s (HandlerT s IO))
            , YesodPersist s
            , Yesod s
            , RenderMessage s FormMessage
            , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
            , RedirectUrl s (Route s)
            )
         => Grid s p c
         -> Key p
         -> HandlerT s IO Html
formGet  = formGen False
formPost = formGen True

formGen :: ( Bounded c
           , Enum c
           , Eq c
           , YesodPersist s
           , Yesod s
           , RenderMessage s FormMessage
           , PersistEntity p
           , PersistQuery (YesodPersistBackend s (HandlerT s IO))
           , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
           ) 
        => Bool 
        -> Grid s p c 
        -> Key p 
        -> HandlerT s IO Html
formGen  r g pid = gridForm r g . Right $ Just pid
formDelete g pid = do
    p <- runDB $ get pid
    case p of
        Nothing -> setMessage noneFoundMsg
        Just _  -> do
            runDB $ delete pid -- how tell if delete succeeded?  what makes sure this was on the right table?
            setMessage "success"
    redirect . groupR $ routes g

noneFoundMsg :: Html
noneFoundMsg = "no object for that id"

makeStatic p = StaticR $ StaticRoute (T.pack <$> splitOn "/" p) []

gridForm :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery ((YesodPersistBackend s) (HandlerT s IO))
            , YesodPersist s
            , Yesod s
            , RenderMessage s FormMessage
            , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
            )
         => Bool
         -> Grid s p c
         -> Either p (Maybe (Key p)) -- left: creating new with default, right: possibly editing
         -> HandlerT s IO Html
gridForm run g sel = do
    let fields  = (id &&& getField g) <$> [minBound..maxBound]
        rts     = routes g
        goGroup = redirect $ groupR rts
        postR   = (const (newR rts) ||| (indR rts . fromJust)) sel 
        False +> _ = Nothing
        True  +> x = x
        (allowActions, title') = case sel of 
            Left        _  -> (False, "create new"    )
            Right (Just _) -> (False, "edit item"     )
            Right Nothing  -> (True , toHtml $ title g)
        (dels, edits) = first (allowDelete g +>) . join (***) ((allowActions +>) . Just) $ (deleteR &&& indR) rts

        done x = defaultLayout $ do
            setTitle title'
            case x of
                Left w -> w
                Right (w, e) -> [whamlet|
<form method=post action=@{postR} enctype=#{e}>
    ^{w}
|]
            when (isJust (defaultNew g) && (const False ||| isNothing) sel)
                [whamlet|
<a href=@{newR rts}> 
        <input type="button" value="new">
|]
    items  <- runDB . selectList [] $ opts g
    rows   <- mapM (makeRow fields dels edits) items
    gridID <- newIdent

    let sel' = right ((\y -> filter ((y ==) . entityKey) items) <$>) sel
        
    case sel' of Right (Just []     ) -> setMessage noneFoundMsg >> goGroup
                 Right (Just (_:_:_)) -> setMessage "error: multiple matches with that id"
                 _ -> return undefined

    case editForm (groupR rts) fields rows sel gridID (jsgrid g) $ (Just ||| (entityVal . head <$>)) sel' of 
        Left  grid -> done . Left $ grid
        Right form -> if run 
            then do
                ((r, w), e) <- runFormPost form
                case r of 
                    FormSuccess p -> do
                        flip (either . const . void . runDB $ insert p) sel $ \(Just pid) -> do -- how tell if insert succeeded?
                            let convert (GridField _ extract' _ (Just (Editable _ pField' True _))) = Just $ pField' =. extract' p
                                convert _ = Nothing
                            runDB . update pid . catMaybes $ convert . getField g <$> [minBound..maxBound] -- how tell if update succeeded?                                  
                        setMessage "success"
                        redirect $ groupR rts -- goGroup    
                    _ -> done $ Right (w, e) -- use contents of failure somehow?  does fvErrors already get it all?
            else do
                (w, e) <- generateFormPost form
                done $ Right (w, e)

makeGrid :: ( -- ToWidget s Html
--              PersistEntity p
--            , PersistQuery (YesodPersistBackend m) (GHandler s m)
--            , YesodPersist m
--            , Bounded c
--            , Enum c
--            , Eq c
--            , PersistEntityBackend p ~ YesodPersistBackend m
--            , RenderMessage m FormMessage  
            ) 
         => Either p (Maybe (Key p)) 
         -> [(c, GridField s p c)]
         -> [(Key p, WidgetT s IO (), [(String, (Maybe (Route s), String))])]
         -> Text                  
         -> Maybe p         
         -> Route s
         -> Maybe (c -> Maybe (FieldView s))
         -> Maybe Text
         -> Maybe Html 
         -> WidgetT s IO ()
makeGrid sel fields rows gridID this groupR' getView tstyle extra = case tstyle of
    Nothing -> dgrid gridID fields rows sel getView extra
    Just tstyle' -> do
        let style = [lucius| .errors { color:red } input { width:auto; margin:0 } form { margin:0 } |]
            form  = [whamlet|
$forall (c, f) <- fields
    <td>
        $maybe view <- (fromJust getView) c
            $#adapted from renderDivs
            <div :fvRequired view:.required :not $ fvRequired view:.optional>
                <label for=#{fvId view}>
                    $maybe tt <- fvTooltip view
                        <div .tooltip>#{tt}
                    ^{fvInput view}
                    $maybe err <- fvErrors view
                        <div .errors >#{err}
        $nothing
            ^{toWhamlet $ mini f (fromJust this)}
<td>
    <input type="submit" value="save">
<td>
    <a href=@{groupR'}> 
        <input type="button" value="cancel">    
|]                
        [whamlet|
^{style}
$maybe ex <- extra
    ^{ex}
<table style=#{tstyle'} id=#{gridID}>
    <thead>
        <tr>
            $forall (c, f) <- fields
                <th> #{heading f $ c}
            $# <th> actions
            $if tstyle' == ""
                <th> edit
                <th> delete
    <tbody>
        $forall (rkey, row, _) <- rows
            <tr>        
                $maybe key <- (const Nothing ||| id) sel
                    $if key == rkey
                        ^{form}
                    $else
                        ^{row}
                $nothing
                    ^{row}  
        $if (const True ||| const False) sel
            <tr>
                ^{form}
|]

editForm :: ( -- ToWidget s Html
--            , PersistEntity p
--            , PersistQuery (YesodPersistBackend m) (GHandler s m)
--            , YesodPersist m
--            , Bounded c
--            , Enum c
              Eq c
--            , PersistEntityBackend p ~ YesodPersistBackend m
            , RenderMessage s FormMessage
            , MonadHandler m
            , s ~ HandlerSite m
            ) 
         => Route s
         -> [(c, GridField s p c)]
         -> [(Key p, WidgetT s IO (), [(String, (Maybe (Route s), String))])]
         -> Either p (Maybe (Key p)) 
         -> Text
         -> Maybe (JSGrid s)
         -> Maybe p
         -> Either (WidgetT s IO ()) 
                   (Html -> MForm m ( FormResult p
                                    , WidgetT s IO ()
                                    )
                   )                 
editForm groupR' fields rows sel gridID jsgrid' this' = do
    let w getView = do
        let htmlGrid = makeGrid sel fields rows gridID this' groupR' getView
        case jsgrid' of
            Nothing -> htmlGrid $ Just "border-spacing: 20px 0; border-collapse: separate"
            Just (DGrid script attrs) -> \extra -> do
                addScriptAttrs script attrs
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
                htmlGrid Nothing extra
            Just (JQGrid (sheets, scripts)) -> \extra -> do
                mapM_ addStylesheet sheets
                toWidget [lucius|
html, body {
    margin: 0;
    padding: 0;
//  font-size: 75%;
}
.ui-jqgrid tr.jqgrow td {white-space:normal;}
|]
                addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"
             -- addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.9.1/jquery-ui.min.js"
                mapM_ addScript scripts
                toWidget $ [julius|
$(document).ready(function() {
    jQuery.extend(jQuery.jgrid.defaults, { altRows:true, height:"auto" });
    tableToGrid("##{rawJS gridID}", { shrinkToFit:true, width:600  }) 
});                
|]

                htmlGrid (Just "") extra
    case this' of 
        Nothing   -> Left $ (w Nothing) Nothing
        Just this -> Right $ \extra -> do
            (rs, vs) <- getDefaultedViews fields this
            return (rs, w (Just $ fromJust . (`lookup` vs)) $ Just extra)

makeRow :: ( -- Bounded c
--           , Enum c
--           , Eq c
--           , PersistEntity p
--           , PersistQuery (YesodPersistBackend m) (GHandler s m)
--           , YesodPersist m
--           , Yesod m
            RenderMessage s FormMessage
--           , PersistEntityBackend p ~ YesodPersistBackend m
           )
        => [(c, GridField s p c)]
        -> Maybe (Key p -> Route s)
        -> Maybe (Key p -> Route s)
        -> Entity p
        -> HandlerT s IO (Key p, WidgetT s IO (), [(String, (Maybe (Route s), String))])        
makeRow fields allowDelete' allowEdit i = do
    let disp x = mini x . entityVal
        raw    = [whamlet|
$forall (_, f) <- fields
    <td>
        ^{toWhamlet $ disp f i}
$# -- TODO: if no fields editable, don't show this (or render action column, or allow item routes)
$maybe indR' <- allowEdit
    <td>
        <a href=@{indR' $ entityKey i}> edit 
|]
    (w, e) <- generateFormPost $ \extra -> return (pure undefined, [whamlet| ^{extra} |]) 
    let widget = [whamlet|
^{raw}
<td>
    $maybe deleteR' <- allowDelete'
        $# how specify method DELETE?  could we somehow POST without a form?
        <form method=post action=@{deleteR' $ entityKey i} enctype=#{e}>
            ^{w}
            <input type="submit" value="delete">
|]
    let jsob = ((\(c,f) -> heading f $ c) &&& (flip disp i . snd)) <$> fields
    return (entityKey i, widget, jsob)

mini (GridField _ extract' display' _) = ((((\x -> (Nothing, x)) .) ||| id) display') . extract'

showFieldByID :: ( YesodPersist s
                 , PersistStore ((YesodPersistBackend s) (HandlerT s IO))
                 , PersistEntity p
                 , PersistEntityBackend p ~ PersistMonadBackend (YesodPersistBackend s (HandlerT s IO))
                 ) 
              => Maybe (Key p -> Route s)
              -> (b -> String)
              -> (p -> b)
              -> Key p
              -> HandlerT s IO (Maybe (Route s), String)
showFieldByID r s f id' = do
    x <- s . f . fromJust <$> (runDB $ get id')
    return (r <*> pure id', x)
                  
toWhamlet :: (Maybe (Route s), String) 
          -> WidgetT s IO ()
toWhamlet (r, x) = do
    let out = [whamlet|
<div style=white-space:pre>#{x}
|]
    [whamlet|
$maybe route <- r
    <a href=@{route}>
        ^{out}
$nothing
    ^{out}
|]

toJulius (r, x) = case r of 
    Nothing    -> [julius|                   escapeHTML("#{x}") |]
    Just route -> [julius| <a href=@{route}> escapeHTML("#{x}") |]

getDefaultedViews :: ( RenderMessage s FormMessage
                     , MonadHandler m
                     , s ~ HandlerSite m
                     )
                  => [(c, GridField s p c)]
                  -> p
                  -> MForm m ( FormResult p
                             , [(c, Maybe (FieldView s))]
                             )               
getDefaultedViews fields this = do
    let defView (mp, cs) (c, g) = second (\x -> (c, x) : cs) <$> 
            case g of 
                GridField _ extract' _ (Just (Editable f _ True updater')) -> do
                     (r, v) <- mreq f "unused"{fsAttrs=[("size","20")]} . Just $ extract' this -- TODO: handle optional fields
                     return (updater' <$> r <*> mp, Just v )
                _ -> return (                   mp, Nothing)
    foldM defView (pure this, []) fields

dgrid = undefined
{-
dgrid :: Text
      -> [(c, GridField s p c)]
      -> [(Key p, WidgetT s IO (), [(String, (Maybe (Route s), String))])]
      -> Either p (Maybe (Key p))
      -> Maybe x
      -> Maybe Html
      -> WidgetT s IO ()
dgrid gridID fields rows sel getView extra = do 
    let -- could probably use http://hackage.haskell.org/packages/archive/aeson/latest/doc/html/Data-Aeson.html#t:ToJSON
        gData   = mconcat $ (\(_,_,r) -> [julius|                   {^{doRow r}                                       }, |]) <$> rows
        doRow r = mconcat $ (\(c,f)   -> [julius| #{heading f $ c}:  ^{toJulius (fromJust $ lookup (heading f $ c) r) }, |]) <$> fields
{-
             $forall (_,_,_) <- rows
                 {
                 $forall (c,f) <- fields
                     #{heading f $ c}:"placeholder",
                 },
-}       
        gCols = mconcat $ (\(c,f) -> [julius| #{heading f $ c}: { label: "#{heading f $ c}"
                                                                , renderCell: function (row, value, td, options){
                                                                    td.innerHTML = value; //so links render
                                                                    td.setAttribute("style","white-space:pre");
                                                                    //console.log(td)
                                                                  }
                                                                }, 
                                     |]) <$> fields
{-
                $forall (c,f) <- fields
                    $with h <- heading f $ c
                        #{h}: { label: "#{h}"},
-}
        dgs = [julius|
require(["dojo/_base/declare", "dgrid/Grid", "dgrid/Keyboard", "dgrid/Selection", "dojo/dom", "dojo/fx", "dojo/request", "dojo/domReady!"],
    function(declare, Grid, Keyboard, Selection, dom, fx, request){
        var data = [
            ^{gData}

            // { first: "Bob"  , last: "Barker", age: 89 },
            // { first: "Vanna", last: "White" , age: 55 },
            // { first: "Pat"  , last: "Sajak" , age: 65 }            
        ];

        // Create a new constructor by mixing in the components
        var CustomGrid = declare([ Grid, Keyboard, Selection ]);
 
        // Now, create an instance of our custom grid which
        // have the features we added!
        var grid = new CustomGrid({
            columns: {
                ^{gCols}

                // first: {
                //     label: "First Name"
                // },
                // last: {
                //     label: "Last Name"
                // },
                // age: {
                //     label: "Age",
                //     renderCell: function(row, value, td, options){
                //         console.log(row);
                //         console.log(value);
                //         console.log(td);
                //         td.innerHTML = "<h1>hi " + value.toString() + "</h1>";
                //         //console.log(options)
                //     } 
                // }
            },
            selectionMode: "single", // for Selection; only select a single row at a time
            cellNavigation: false // for Keyboard; allow only row-level keyboard navigation
        }, #{gridID});

        grid.renderArray(data);

        grid.on(".dgrid-row:click", function(event){
            console.log("Row clicked:", grid.row(event).id);
 
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

//http://blog.nickburwell.com/2011/02/escape-html-tags-in-javascript.html
function escapeHTML( string ){
    var pre = document.createElement('pre');
    var text = document.createTextNode( string );
    pre.appendChild(text);
    return pre.innerHTML;
}

|]

    [whamlet| 
^{dgs}
<div id=#{gridID}>
|]
-}
