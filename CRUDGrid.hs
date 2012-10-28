{-# LANGUAGE ExistentialQuantification, TupleSections #-}

module CRUDGrid
    ( Grid      (..)
    , GridField (..)
    , Editable  (..)
    , Routes    (..)
    , groupGet
    , formGet
    , formPost
    ) where

import Import
import Control.Applicative
import Data.Maybe
import Data.Traversable (sequenceA)
import Control.Monad
import Prelude (head)

type ID m p = Key (YesodPersistBackend m) p

data Routes m p =
    Routes { indR   :: ID m p -> Route m
           , groupR :: Route m
           }

data Grid s m p c =
    Grid { title    :: Text
         , routes   :: Routes m p
         , getField :: c -> GridField s m p c
         }

data GridField s m p c = forall t. 
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: t -> String
            , editable :: Maybe (Editable s m t p)
            }

data Editable s m t p = forall tv.
  Editable { vField   :: Field s m tv
           , pField   :: EntityField p t
           , v2p      :: tv -> Either Text t
           , p2v      :: t -> tv
           , required :: Bool
           }

groupGet :: ( Bounded c
            , Enum c
            , Eq c
            , Show c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Grid s m p c
         -> GHandler s m RepHtml
groupGet g = defaultLayout . (setTitle (toHtml $ title g) >>) . fst =<< generateFormPost =<< fst <$> makeGrid g Nothing

formGet, formPost
         :: ( Bounded c
            , Enum c
            , Eq c
            , Show c
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
formGet  = gridForm False
formPost = gridForm True

-- run a form, extract results, and update the database
gridForm :: ( Bounded c
            , Enum c
            , Eq c
            , Show c
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Yesod m
            , RenderMessage m FormMessage
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => Bool
         -> Grid s m p c
         -> ID m p
         -> GHandler s m RepHtml
gridForm run g pid = do
    (form, routes) <- makeGrid g $ Just pid
    let done w e = defaultLayout $ do
        setTitle "editing item"
        [whamlet|
<form method=post action=@{indR routes $ pid} enctype=#{e}>
    ^{w}
|]  
    if run
        then do
            ((r, w), e) <- runFormPost form
            case r of
                FormSuccess rs -> do
                    -- runDB $ update pid . getZipList $ ZipList ((=.) <$> dbField <$> snd <$> getEditable fields) <*> ZipList rs
                    setMessage "success"
                    redirect $ groupR routes
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost form
            done w e

getDefaultedViews :: ( RenderMessage m FormMessage
                     , Show c
                     )
                  => Maybe (Key (PersistEntityBackend p) p)
                  -> [Entity p]
                  -> [(c, GridField s m p c)]
                  -> MForm s m ( t -- FormResult t
                               , [(c, Maybe (FieldView s m))]
                               )
getDefaultedViews sel items fields = do
    let this  = entityVal . head <$> (\x -> filter ((x ==) . entityKey ) items) <$> sel
        defView (GridField _ extract _ (Just (Editable v _ _ p2v True))) = Just <$> snd <$> mreq v "unused" ((p2v . extract) <$> this) -- TODO: handle optional fields
        defView _ = return Nothing
    vs <- mapM (\(c, f) -> (c,) <$> defView f) fields
    return (undefined, vs)
    --TODO: get FormResults working! sequenceA, unzip, PersistEntity p

-- generate a grid showing all items of a given type, possibly including a form for editing a selected one
makeGrid :: ( PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Bounded c
            , Enum c
            , Eq c
            , Show c
            , PersistEntityBackend p ~ YesodPersistBackend m
            , RenderMessage m FormMessage
            ) 
         => Grid s m p c
         -> Maybe (ID m p)
         -> GHandler s m ( Html -> MForm s m (t, GWidget s m ())
                         , Routes m p
                         )
makeGrid g sel = do
    items <- runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    return . (, routes g) $ \extra -> do
        let fields = (\x -> (x, getField g $ x)) <$> [minBound..maxBound]
            disp (GridField _ extract display _) = display . extract . entityVal
            dispRow i = [whamlet|
$forall (_, f) <- fields
    <td>
        #{disp f i}
<td>
    $# -- TODO: if no fields editable, don't show this (or render action column, or allow item routes)
    <a href=@{(indR $ routes g) $ entityKey i}> edit 
|]
        (rs, vs) <- getDefaultedViews sel items fields
        let getView c = fromJust $ lookup c vs
            style  = [lucius| .errors { color:red } |]
            widget = [whamlet|
^{style}
^{extra}
<table>
    <thead>
        <tr>
            $forall (c, _) <- fields
                <th> #{(heading $ getField g $ c) c}
            <th> actions
    <tbody>
        $forall i <- items
            <tr>        
                $maybe key <- sel
                    $if key == entityKey i
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
                                        #{disp f i}
                            <td>
                                <input type="submit" value="save">
                                <a href=@{groupR $ routes g}> 
                                    <input type="button" value="cancel">
                    $else
                        ^{dispRow i}
                $nothing
                    ^{dispRow i}  
|]
        return (rs, widget)