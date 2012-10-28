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
import Prelude (head)
import Data.Either
import qualified Data.Text.IO as T

type ID m p = Key (YesodPersistBackend m) p

data Routes m p =
    Routes { indR   :: ID m p -> Route m
           , groupR :: Route m
           }

data Grid s m p c t tv =
    Grid { title    :: Text
         , routes   :: Routes m p
         , getField :: c -> GridField s m p c t tv
         }

data GridField s m p c t tv =
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: t -> String
            , editable :: Maybe (Editable s m t p tv)
            }

data Editable s m t p tv =
  Editable { vField   :: Field s m tv
           , pField   :: EntityField p t
           , v2p      :: tv -> Either Text t
           , p2v      :: t -> tv
           , required :: Bool
           }

{-
we need to hide the types t and tv, or else we can't work with a grid that has fields of more than one type.
the following definitions work up to the point of getting a bunch of FormResult a from mreq.
i don't know how to package those up in a way that won't expose their heterogeneous a's.
possibly have user pass in something that will know how to use them and hide their values from us?

in order to demonstrate actually writing to the database, i'm temporarily disabling these defs (they have to be updated in all the type signatures when changed).

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
-}

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
         => Grid s m p c t tv
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
            , PersistField t
            )
         => Grid s m p c t tv
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
            , PersistField t
            )
         => Bool
         -> Grid s m p c t tv
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
                    -- TODO: this spot won't work when we hide the types t and tv, but written as is, we can only have fields of one type
                    -- user should pass something in to the Grid that makeGrid will use to process the results/hide the types and pass that to us
                    let getFns (GridField _ _ _ (Just (Editable _ pField v2p _ True))) = Just (pField, v2p)
                        getFns _ = Nothing
                        (pfs, converters) = unzip . catMaybes $ getFns . getField g <$> [minBound..maxBound]
                        (bads, goods) = partitionEithers $ getZipList $ ZipList converters <*> ZipList rs
                    if null bads
                        then do
                            runDB $ update pid . getZipList $ ZipList ((=.) <$> pfs) <*> ZipList goods
                            setMessage "success"
                            redirect $ groupR routes
                        else do
                            liftIO $ mapM_ T.putStrLn bads
                            setMessage "no data updated"
                            undefined -- TODO show errors, reload form without undoing their edits?                    
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost form
            done w e

getDefaultedViews :: ( RenderMessage m FormMessage
                     , Show c
                     )
                  => Maybe (Key (PersistEntityBackend p) p)
                  -> [Entity p]
                  -> [(c, GridField s m p c t tv)]
                  -> MForm s m ( FormResult [tv]
                               , [(c, Maybe (FieldView s m))]
                               )
getDefaultedViews sel items fields = do
    let this  = entityVal . head <$> (\x -> filter ((x ==) . entityKey ) items) <$> sel
        -- TODO: this spot won't work when we hide the types t and tv, but written as is, we can only have fields of one type
        -- defView works when only giving out the snd result (FieldView) of mreq, because it hides t and tv.  
        -- but we need the fst result (FormResult a) to process results -- but that exposes tv!
        defView (GridField _ extract _ (Just (Editable v _ _ p2v True))) = Just <$> mreq v "unused" (p2v . extract <$> this) -- TODO: handle optional fields
        defView _ = return Nothing
    (cs, mrsvs) <- unzip <$> mapM (\(c, f) -> (c,) <$> defView f) fields
    let (rs, vs) = unzip $ zipWith (\c m -> maybe (Nothing, (c, Nothing)) (\(a, b) -> (Just a, (c, Just b))) m) cs mrsvs -- ugly!  must be better way...
    return (sequenceA $ catMaybes rs, vs)

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
         => Grid s m p c t tv
         -> Maybe (ID m p)
         -> GHandler s m ( Html -> MForm s m ( FormResult [tv]
                                             , GWidget s m ()
                                             )
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