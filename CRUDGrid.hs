{-# LANGUAGE ExistentialQuantification, TupleSections #-}

module CRUDGrid
    ( Grid      (..)
    , GridField (..)
    , Editable  (..)
    , Routes    (..)
    , groupGet
    , formGet
    , formPost
    , formDelete
    , formNewGet
    , formNewPost
    ) where

import Import
import Control.Arrow
import Control.Monad
import Data.Maybe
import Prelude (head)

type ID m p = Key (YesodPersistBackend m) p

data Routes m p =
    Routes { indR    :: ID m p -> Route m
           , groupR  :: Route m
           , newR    :: Route m
           , deleteR :: ID m p -> Route m
           }

data Grid s m p c =
    Grid { title       :: Text
         , allowDelete :: Bool
         , defaultNew  :: Maybe p
         , routes      :: Routes m p
         , getField    :: c -> GridField s m p c
         }

data GridField s m p c = forall t. (PersistField t) =>
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: t -> String
            , editable :: Maybe (Editable s m t p)
            }

data Editable s m t p = 
  Editable { vField   :: Field s m t
           , pField   :: EntityField p t
           , required :: Bool
           , updater  :: t -> Maybe p -> Maybe p -- required to avoid "Record update for insufficiently polymorphic field"
                                                 -- could be t -> p -> p if FormResult were Traversable
           }

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
groupGet g = defaultLayout . (setTitle (toHtml $ title g) >>) . fst =<< generateFormPost =<< fst <$> makeGrid g (Right Nothing) -- unkosher use of generateFormPost? 
formNewGet  = formNewPostGen False
formNewPost = formNewPostGen True
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
formGen  r g pid = gridForm r g $ Right pid
formDelete g pid = do
    p <- runDB $ get pid
    case p of
        Nothing -> setMessage "no object for that id"
        Just _  -> do
            runDB $ delete pid -- how tell if delete succeeded?  what makes sure this was on the right table?
            setMessage "success"
    redirect . groupR $ routes g

-- run a form, extract results, and update the database
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
         -> Either p (ID m p)
         -> GHandler s m RepHtml
gridForm run g sel = do
    (form, routes) <- makeGrid g $ (id +++ Just) sel
    let postR    = (const (newR routes) ||| indR routes) sel 
        done w e = defaultLayout $ do
            setTitle "editing item"
            [whamlet|
<form method=post action=@{postR} enctype=#{e}>
    ^{w}
|]  
    if run 
        then do
            ((r, w), e) <- runFormPost form
            case r of 
                FormSuccess (Just p) -> do
                    flip (either . const . void . runDB $ insert p) sel $ \pid -> do -- how tell if insert succeeded?
                        let convert (GridField _ extract _ (Just (Editable _ pField True _))) = Just $ pField =. extract p
                            convert _ = Nothing
                        runDB . update pid . catMaybes $ convert . getField g <$> [minBound..maxBound] -- how tell if update succeeded?                                  
                    setMessage "success"
                    redirect $ groupR routes       
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost form
            done w e

getDefaultedViews :: ( RenderMessage m FormMessage
                     )
                  => [(c, GridField s m p c)]
                  -> Maybe p
                  -> MForm s m ( FormResult (Maybe p)
                               , [(c, Maybe (FieldView s m))]
                               )
getDefaultedViews fields this = do
    let defView (mp, cs) (c, g) = second (\x -> (c, x) : cs) <$> 
            case g of 
                GridField _ extract _ (Just (Editable f _ True updater)) -> do
                     (r, v) <- mreq f "unused" $ extract <$> this -- TODO: handle optional fields
                     return (updater <$> r <*> mp, Just v )
                _ -> return (                  mp, Nothing)
    foldM defView (pure this, []) fields

{- makes a handler instead of a widget
delForm i = do
    (w, e) <- generateFormPost . renderDivs $ areq textField "stub" Nothing

    [whamlet|
<form method=post action=@{i} enctype=#{e}>
    ^{w}
<input type="submit" value="delete">
|]
-}

--hack to use GET
delForm i = [whamlet|
<a href=@{i}>
    <input type="button" value="delete">
|]

-- generate a grid showing all items of a given type, possibly including a form for editing a selected one
makeGrid :: ( PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Bounded c
            , Enum c
            , Eq c
            , PersistEntityBackend p ~ YesodPersistBackend m
            , RenderMessage m FormMessage
            ) 
         => Grid s m p c
         -> Either p (Maybe (ID m p)) -- left: creating new with default, right: possibly editing
         -> GHandler s m ( Html -> MForm s m ( FormResult (Maybe p)
                                             , GWidget s m ()
                                             )
                         , Routes m p
                         )
makeGrid g sel = do
    items <- runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    let fields = (id &&& getField g) <$> [minBound..maxBound]
    return . (, routes g) $ \extra -> do
        (rs, vs) <- getDefaultedViews fields . flip (either $ Just . id) sel $ (\x -> entityVal . head <$> (\y -> filter ((y ==) . entityKey) items) <$> x) -- TODO: redirect if not found
        let getView  = fromJust . (`lookup` vs)
            style    = [lucius| .errors { color:red } |]
            disp x   = mini x . entityVal
            nediting = (const False ||| isNothing) sel
            mini (GridField _ extract display _) = display . extract
            dispRow i = [whamlet|
$forall (_, f) <- fields
    <td>
        #{disp f i}
$# -- TODO: if no fields editable, don't show this (or render action column, or allow item routes)
$if nediting
    <td>
        <a href=@{(indR $ routes g) $ entityKey i}> edit 
    $if allowDelete g
        <td>
            $# how specify method shouldn't be GET?
            ^{delForm $ (deleteR $ routes g) $ entityKey i}                
|]
            form t = [whamlet|
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
            $maybe i <- t
                #{disp f i}
            $nothing
                #{mini (getField g $ c) $ (id ||| undefined) sel}
<td>
    <input type="submit" value="save">
<td>
    <a href=@{groupR $ routes g}> 
        <input type="button" value="cancel">
|]
            widget = [whamlet|
^{style}
^{extra}
<table>
    <thead>
        <tr>
            $forall (c, f) <- fields
                <th> #{heading f $ c}
            $# <th> actions
    <tbody>
        $forall i <- items
            <tr>        
                $maybe key <- (const Nothing ||| id) sel
                    $if key == entityKey i
                        ^{form $ Just i}
                    $else
                        ^{dispRow i}
                $nothing
                    ^{dispRow i}  
        $if (const True ||| const False) sel
            <tr>
                ^{form Nothing}
$if (&&) nediting $ isJust $ defaultNew g
    <a href=@{newR $ routes g}> 
        <input type="button" value="new">
|]
        return (rs, widget)