module GridField
    ( Gridder
    , GridField (..)
    , Editable (..)
    , groupGet
    , formGet
    , formPost
    , itemGrid
    ) where

import Import
import Text.Blaze
import Control.Applicative
import Data.Maybe
import Data.Traversable (sequenceA)
import Control.Monad
import Prelude (head)

type ID m p = Key (YesodPersistBackend m) p

type Gridder s m t p = 
	   Maybe (ID m p)
	-> GHandler s m ( GHandler s m (Markup -> MForm s m (FormResult [t], GWidget s m ()))
                    , ID m p -> Route m
                    , Route m
                    , [GridField s m t p]
                    )

data GridField s m t p = 
  GridField { label    :: Text
            , extract  :: p -> String
            , editable :: Maybe (Editable s m t p)
            , needShow :: Bool
            }

data Editable s m t p =
  Editable { uiField  :: Field s m t
           , dbField  :: EntityField p t
           , required :: Bool
           }

groupGet :: ( RenderMessage m FormMessage
            , Yesod m
            )
         => Gridder s m t p
         -> GHandler s m RepHtml
groupGet f = do
    (grid, _, _, _) <- f Nothing
    defaultLayout . fst =<< generateFormPost =<< grid -- unkosher use of generateFormPost?  how include a 'setTitle'?

formGet, formPost
         :: ( PersistEntity p
            , RenderMessage m FormMessage
            , Yesod m
            , YesodPersist m
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , PersistField t
            )
         => Gridder s m t p
         -> ID m p
         -> GHandler s m RepHtml
formGet  = gridForm False
formPost = gridForm True

-- lookup all items of a given type in the database
itemGrid :: ( PersistEntity p
            , RenderMessage m FormMessage
            , YesodPersist m
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , Read t
            , PersistEntityBackend p ~ YesodPersistBackend m
            )
         => (ID m p -> Route m)
         -> Route m
         -> [GridField s m t p]
         -> Gridder s m t p
itemGrid indR groupR fields sel = do
    items <- runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    let grid = makeGrid indR groupR sel items fields
    return (grid, indR, groupR, fields)

-- run a form, extract results, and update the database
gridForm :: ( Yesod m
            , YesodPersist m
            , PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , PersistField t
            , RenderMessage m FormMessage
            )
         => Bool
         -> Gridder s m t p
         -> ID m p
         -> GHandler s m RepHtml
gridForm post gridder pid = do
    (grid, indR, groupR, fields) <- gridder $ Just pid
    let done w e = defaultLayout $ do
        setTitle "editing item"
        [whamlet|
<form method=post action=@{indR pid} enctype=#{e}>
    ^{w}
|]   
    if post 
        then do
            ((r, w), e) <- runFormPost =<< grid
            case r of
                FormSuccess rs -> do
                    runDB $ update pid . getZipList $ ZipList ((=.) <$> dbField <$> snd <$> getEditable fields) <*> ZipList rs
                    setMessage "success"
                    redirect groupR 
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost =<< grid
            done w e
      
-- TODO: split these up into requireds and optionals so we can keep their form results/types separate
getEditable fs = (\x -> (x, fromJust $ editable x)) <$> filter (isJust . editable) fs

getDefaultedViews :: ( Read t
                     , PersistEntity p
                     , RenderMessage m FormMessage
                     )
                  =>  Maybe (Key (PersistEntityBackend p) p)
                  -> [Entity p]
                  -> [GridField s m t p]
                  -> MForm s m ( FormResult [t]
                               , [(Text, FieldView s m)]
                               )
getDefaultedViews sel items fields = do
    let this  = entityVal . head <$> (\x -> filter ((x ==) . entityKey ) items) <$> sel
        (e,f) = unzip $ getEditable fields
--  to mix required/optional, need separate lists from getEditable to share with gridForm (who uses them to write to the db)
--  (rs, vs) <- unzip <$> zipWithM (\x -> (if required x then mreq else mopt) (uiField x) "unused" $ extract y <$> this) f e
--  have to hack defaulting w/read until we can have heterogeneous list of extractors
    (rs, vs) <- unzip <$> zipWithM (\x y -> mreq (uiField x) "unused" $ read . extract y <$> this) f e
    return (sequenceA rs, zip (label <$> e) vs)

-- generate a grid showing the fields for items passed in, possibly including a form for editing a selected one
makeGrid :: ( Read t
            , PersistEntity p
            , RenderMessage m FormMessage
            )
         => (Key (PersistEntityBackend p) p -> Route m)
         -> Route m
         -> Maybe (Key (PersistEntityBackend p) p)
         -> [Entity p]
         -> [GridField s m t p]
         -> GHandler s m (Markup -> MForm s m (FormResult [t], GWidget s m ()))
makeGrid indR groupR sel items fields = return $ \extra -> do
            (rs, vs) <- getDefaultedViews sel items fields
            let getView f = fromJust $ lookup (label f) vs -- won't be called if can't find...
                disp i f  = (if needShow f then show else id) $ extract f $ entityVal i
                dispRow i = [whamlet|
$forall f <- fields
    <td>
        #{disp i f}
<td>
    <a href=@{indR $ entityKey i}> edit  
|]
                style  = [lucius| .errors { color:red } |]
                widget = [whamlet|
^{style}
^{extra}
<table>
    <thead>
        <tr>
            $forall f <- fields
                <th> #{label f}
            <th> actions
    <tbody>
        $forall i <- items
            <tr>        
                $maybe key <- sel
                    $if key == entityKey i
                            $forall f <- fields
                                <td>
                                    $maybe _ <- editable f
                                        $with view <- getView f
                                            $#adapted from renderDivs
                                            <div :fvRequired view:.required :not $ fvRequired view:.optional>
                                                <label for=#{fvId view}>
                                                    $maybe tt <- fvTooltip view
                                                        <div .tooltip>#{tt}
                                                    ^{fvInput view}
                                                    $maybe err <- fvErrors view
                                                        <div .errors >#{err}
                                    $nothing
                                        #{disp i f}
                            <td>
                                <input type="submit" value="save">
                                <a href=@{groupR}> 
                                    <input type="button" value="cancel">
                    $else
                        ^{dispRow i}
                $nothing
                    ^{dispRow i}                                
|]
            return (rs, widget)