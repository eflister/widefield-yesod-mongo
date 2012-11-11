{-# LANGUAGE ExistentialQuantification #-}

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

import Import
import Control.Arrow
import Control.Monad
import Data.Maybe
import Prelude (head)
import Data.List.Split
import qualified Data.Text as T

type ID m p = Key (YesodPersistBackend m) p

data Routes m p =
    Routes { indR    :: ID m p -> Route m
           , groupR  :: Route m
           , newR    :: Route m
           , deleteR :: ID m p -> Route m
           }

data Grid s m p c = 
    Grid { title       :: Text
         , opts        :: [SelectOpt p] -- need some way to sort on indirect fields
         , allowDelete :: Bool
         , defaultNew  :: Maybe p
         , routes      :: Routes m p
         , jsgrid      :: Maybe (JSGrid m)
         , getField    :: c -> GridField s m p c
         }

data GridField s m p c = forall t. (PersistField t) =>
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: Either (t -> String) (t -> (Maybe (Route m), String))
            , editable :: Maybe (Editable s m t p)
            }

data Editable s m t p = 
  Editable { vField   :: Field s m t
           , pField   :: EntityField p t
           , required :: Bool
           , updater  :: t -> p -> p -- required to avoid "Record update for insufficiently polymorphic field"
           }

data JSGrid m = JQGrid ([Route m], [Route m])
              | DGrid (Route m) [(Text, Text)]

groupGet, formNewGet, formNewPost
         :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Grid s m p c
         -> GHandler s m RepHtml
groupGet g = gridForm False g $ Right Nothing
formNewGet  = formNewPostGen False
formNewPost = formNewPostGen True

formNewPostGen :: ( Bounded c
                  , Enum c
                  , Eq c
                  , YesodPersist m
                  , Yesod m
                  , RenderMessage m FormMessage
                  , PersistEntity b
                  , PersistQuery (PersistEntityBackend b) (GHandler s m)
                  , YesodPersistBackend m ~ PersistEntityBackend b
                  ) 
               => Bool 
               -> Grid s m b c 
               -> GHandler s m RepHtml
formNewPostGen r g = gridForm r g $ Left . fromJust $ defaultNew g

formGet, formPost, formDelete
         :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Grid s m p c
         -> ID m p
         -> GHandler s m RepHtml
formGet  = formGen False
formPost = formGen True

formGen :: ( Bounded c
           , Enum c
           , Eq c
           , YesodPersist m
           , Yesod m
           , RenderMessage m FormMessage
           , PersistEntity p
           , PersistQuery (PersistEntityBackend p) (GHandler s m)
           , YesodPersistBackend m ~ PersistEntityBackend p
           ) 
        => Bool 
        -> Grid s m p c 
        -> ID m p 
        -> GHandler s m RepHtml
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
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Bool
         -> Grid s m p c
         -> Either p (Maybe (ID m p)) -- left: creating new with default, right: possibly editing
         -> GHandler s m RepHtml
gridForm run g sel = do
    let fields  = (id &&& getField g) <$> [minBound..maxBound]
        rts     = routes g
        goGroup = redirect $ groupR rts
        postR   = (const (newR rts) ||| (indR rts . fromJust)) sel 

    items <- runDB . selectList [] $ opts g

    let False +> _ = Nothing
        True  +> x = x
        (allowActions, title') = case sel of 
            Left        _  -> (False, "create new"    )
            Right (Just _) -> (False, "edit item"     )
            Right Nothing  -> (True , toHtml $ title g)
        (dels, edits) = first (allowDelete g +>) $ join (***) (allowActions +>) (Just $ deleteR rts, Just $ indR rts)

    rows <- mapM (makeRow fields dels edits) items

    gridID <- newIdent

    let done w e = defaultLayout $ do
            setTitle title'
            let htmlGrid = makeGrid postR sel fields rows w e gridID
            case jsgrid g of
                        Nothing -> htmlGrid $ Just "border-spacing: 20px 0; border-collapse: separate"
                        Just (DGrid script attrs) -> do
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
                            htmlGrid Nothing
                        Just (JQGrid (sheets, scripts)) -> do
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
                            toWidget [julius| 
$(document).ready(function() {
    jQuery.extend(jQuery.jgrid.defaults, { altRows:true, height:"auto" });
    tableToGrid("##{gridID}", { shrinkToFit:true, width:350  }) 
});                
|]
                            htmlGrid $ Just ""
            when (isJust (defaultNew g) && (const False ||| isNothing) sel)
                [whamlet|
<a href=@{newR rts}> 
        <input type="button" value="new">
|]

    let sel' = right ((\y -> filter ((y ==) . entityKey) items) <$>) sel
        
    case sel' of Right (Just []     ) -> setMessage noneFoundMsg >> goGroup
                 Right (Just (_:_:_)) -> setMessage "error: multiple matches with that id"
                 _ -> return undefined

    case editForm (groupR rts) fields <$> (Just ||| (entityVal . head <$>) $ sel') of 
        Nothing -> done undefined undefined
        Just form ->
            if run 
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
                        _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
                else do
                    (w, e) <- generateFormPost form
                    done w e

makeRow :: ( Bounded c
           , Enum c
           , Eq c
           , PersistEntity p
           , PersistQuery (YesodPersistBackend m) (GHandler s m)
           , YesodPersist m
           , Yesod m
           , RenderMessage m FormMessage
           , PersistEntityBackend p ~ YesodPersistBackend m
           )
        => [(c, GridField s m p c)]
        -> Maybe (ID m p -> Route m)
        -> Maybe (ID m p -> Route m)
        -> Entity p
        -> GHandler s m (ID m p, GWidget s m (), [(String, (Maybe (Route m), String))])
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
        $# how specify method DELETE?
        <form method=post action=@{deleteR' $ entityKey i} enctype=#{e}>
            ^{w}
            <input type="submit" value="delete">
|]
    let jsob = {- toJSON $ -} ((\(c,f) -> heading f $ c) &&& (flip disp i . snd)) <$> fields -- [(String, GWidget s m ())]
    return (entityKey i, widget, jsob)

makeGrid :: ( Bounded c
            , Enum c
            , Eq c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Route m
         -> Either p (Maybe (ID m p))
         -> [(c, GridField s m p c)]
         -> [(ID m p, GWidget s m (), [(String, (Maybe (Route m), String))])]
         -> GWidget s m ()
         -> Enctype
         -> Text
         -> Maybe Text 
         -> GWidget s m ()
makeGrid postR sel fields rows w e gid tstyle = do
    let style = [lucius| .errors { color:red } |]
        form  = [whamlet|
<form method=post action=@{postR} enctype=#{e}>
    ^{w}
|]
        grid = case tstyle of
            Just tstyle' -> [whamlet|
<table style=#{tstyle'} id=#{gid}>
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
            Nothing -> dgrid gid fields rows sel form
    [whamlet|
^{style}
^{grid}
|]

mini (GridField _ extract' display' _) = ((((\x -> (Nothing, x)) .) ||| id) display') . extract'

showFieldByID :: ( YesodPersist m
                 , PersistStore (YesodPersistBackend m) (GHandler s m)
                 , PersistEntity p
                 ) 
              => Maybe (ID m p -> Route m)
              -> (b -> String)
              -> (p -> b)
              -> ID m p
              -> GHandler s m (Maybe (Route m), String)
showFieldByID r s f id' = do
    x <- s . f . fromJust <$> (runDB $ get id')
    return (r <*> pure id', x)

toWhamlet :: (Maybe (Route m), String) 
          -> GWidget s m ()
toWhamlet (r, x) = [whamlet|
$maybe route <- r
    <a href=@{route}> #{x}
$nothing
    #{x} 
|]

{-
toJulius :: (Maybe (Route m), String) 
         -> (Route m -> [t] -> Text) 
         -> Text.Julius.Javascript
-}
toJulius (r, x) = case r of 
    Nothing    -> [julius|                   #{x} |] -- this space shows up in the values!
    Just route -> [julius| <a href=@{route}> #{x} |]

editForm :: ( PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Bounded c
            , Enum c
            , Eq c
            , PersistEntityBackend p ~ YesodPersistBackend m
            , RenderMessage m FormMessage
            ) 
         => Route m
         -> [(c, GridField s m p c)]
         -> p
         -> Html 
         -> MForm s m ( FormResult p
                      , GWidget s m ()
                      )
editForm groupR' fields this extra = do
    (rs, vs) <- getDefaultedViews fields this
    let getView = fromJust . (`lookup` vs)
        w = [whamlet|
^{extra}
$forall (c, f) <- fields
    <td>
        $maybe view <- getView c
            $#adapted from renderDivs
            <div :fvRequired view:.required :not $ fvRequired view:.optional>
                <label for=#{fvId view}>
                    $maybe tt <- fvTooltip view
                        <div .tooltip>#{tt}
                    ^{fvInput view}
                    $maybe err <- fvErrors view
                        <div .errors >#{err}
        $nothing
            ^{toWhamlet $ mini f this}
<td>
    <input type="submit" value="save">
<td>
    <a href=@{groupR'}> 
        <input type="button" value="cancel">
|]
    return (rs, w)

getDefaultedViews :: ( RenderMessage m FormMessage
                     )
                  => [(c, GridField s m p c)]
                  -> p
                  -> MForm s m ( FormResult p
                               , [(c, Maybe (FieldView s m))]
                               )
getDefaultedViews fields this = do
    let defView (mp, cs) (c, g) = second (\x -> (c, x) : cs) <$> 
            case g of 
                GridField _ extract' _ (Just (Editable f _ True updater')) -> do
                     (r, v) <- mreq f "unused" . Just $ extract' this -- TODO: handle optional fields
                     return (updater' <$> r <*> mp, Just v )
                _ -> return (                   mp, Nothing)
    foldM defView (pure this, []) fields

dgrid ::    Text
         -> [(c, GridField s m p c)]
         -> [(ID m p, GWidget s m (), [(String, (Maybe (Route m), String))])]
         -> Either p (Maybe (ID m p))
         -> GWidget s m ()
         -> GWidget s m ()
dgrid gridID fields rows sel form = do
    let -- could probably use http://hackage.haskell.org/packages/archive/aeson/latest/doc/html/Data-Aeson.html#t:ToJSON
        gData   = mconcat $ (\(_,_,r) -> [julius|                   {^{doRow r}                                       }, |]) <$> rows
        doRow r = mconcat $ (\(c,f)   -> [julius| #{heading f $ c}: "^{toJulius (fromJust $ lookup (heading f $ c) r)}", |]) <$> fields
{-
             $forall (_,_,_) <- rows
                 {
                 $forall (c,f) <- fields
                     #{heading f $ c}:"placeholder",
                 },
-}       
        gCols = mconcat $ (\(c,f) -> [julius| #{heading f $ c}: { label: "#{heading f $ c}"
                                                                , renderCell: function (row, value, td, options){
                                                                    td.innerHTML = value; //this is so links render, but causes other angle bracketed strings to unescape as well :(
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

                //first: {
                //    label: "First Name"
                //},
                //last: {
                //    label: "Last Name"
                //},
                //age: {
                //    label: "Age",
                //    renderCell: function(row, value, td, options){
                //        console.log(row);
                //        console.log(value);
                //        console.log(td);
                //        td.innerHTML = "<h1>hi " + value.toString() + "</h1>";
                //        //console.log(options)
                //    } 
                //}
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
|]

    [whamlet| 
^{dgs}
<div id=#{gridID}>
|]