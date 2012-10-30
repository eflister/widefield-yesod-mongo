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
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Either
import Data.Maybe
import qualified Data.Text.IO as T
import Prelude (head)

type ID m p = Key (YesodPersistBackend m) p

data Routes m p =
    Routes { indR   :: ID m p -> Route m
           , groupR :: Route m
           }

data Grid s m p c r 
  = Grid { title    :: Text
         , routes   :: Routes m p
         , getField :: c -> GridField s m p c
         }

data GridField s m p c
  = forall t. (PersistField t) =>
  GridField { heading  :: c -> String
            , extract  :: p -> t
            , display  :: t -> String
            , editable :: Maybe (Editable s m t p)
            }

data Editable s m t p
  = forall tv.
  Editable { vField   :: Field s m tv
           , pField   :: EntityField p t
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
         => Grid s m p c r
         -> GHandler s m RepHtml
groupGet g = defaultLayout . (setTitle (toHtml $ title g) >>) . fst =<< generateFormPost =<< fst <$> makeGrid g Nothing -- unkosher use of generateFormPost?

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
         => Grid s m p c r
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
         -> Grid s m p c r
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
                FormSuccess (Just p) -> do
                    let convert (GridField _ extract _ (Just (Editable _ pField _ True))) = Just $ pField =. extract p
                        convert _ = Nothing
                    runDB . update pid . catMaybes $ convert . getField g <$> [minBound..maxBound]
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
    let defView (mp, cs) (c, g) = case g of 
            GridField _ extract _ (Just (Editable v _ p2v True)) -> do
                 (r, v) <- mreq v "unused" $ p2v . extract <$> this -- TODO: handle optional fields
                 return (mp {- <$> extract = (<*> r) -}, (c, Just v ) : cs)
            _ -> return (mp                            , (c, Nothing) : cs)
    foldM defView (pure this, []) fields

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
         => Grid s m p c r
         -> Maybe (ID m p)
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
        (rs, vs) <- getDefaultedViews fields $ entityVal . head <$> (\x -> filter ((x ==) . entityKey ) items) <$> sel
        let getView = fromJust . flip lookup vs
            style = [lucius| .errors { color:red } |]
            disp (GridField _ extract display _) = display . extract . entityVal
            dispRow i = [whamlet|
$forall (_, f) <- fields
    <td>
        #{disp f i}
<td>
    $# -- TODO: if no fields editable, don't show this (or render action column, or allow item routes)
    <a href=@{(indR $ routes g) $ entityKey i}> edit 
|]
            widget = [whamlet|
^{style}
^{extra}
<table>
    <thead>
        <tr>
            $forall (c, f) <- fields
                <th> #{heading f $ c}
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