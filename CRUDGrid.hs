{-# LANGUAGE ExistentialQuantification #-}

module CRUDGrid
    ( Grid      (..)
    , GridField (..)
    , Editable  (..)
    , Routes    (..)
    , groupGet
    , formGet
    , formPost
    , itemGrid
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
    Grid { routes   :: Routes m p
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

type Gridder s m p t =
       Maybe (ID m p)
    -> GHandler s m (Html -> MForm s m (t, GWidget s m ()))

groupGet :: ( Yesod m
            , RenderMessage m FormMessage
            )
         => Gridder s m p (FormResult r)
         -> GHandler s m RepHtml
groupGet gridder = defaultLayout . fst =<< generateFormPost =<< gridder Nothing -- unkosher use of generateFormPost?  how include a 'setTitle'?

{-
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
-}
formGet, formPost
         :: ( Yesod m
            , RenderMessage m FormMessage
            )
         => Gridder s m p (FormResult r)
         -> ID m p
         -> GHandler s m RepHtml
formGet  = gridForm False
formPost = gridForm True

{-
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
-}
gridForm :: ( Yesod m
            , RenderMessage m FormMessage
            )
         => Bool
         -> Gridder s m p (FormResult r)
         -> ID m p
         -> GHandler s m RepHtml
gridForm run gridder pid = do
    f <- gridder $ Just pid
    let done w e = defaultLayout $ do
        setTitle "editing item"
        [whamlet|
<form method=post  enctype=#{e}>
    ^{w}
|]  {-action=@{indR (routes grid) $ pid}-}
    if run
        then do
            ((r, w), e) <- runFormPost f
            case r of
                FormSuccess rs -> do
                    -- runDB $ update pid . getZipList $ ZipList ((=.) <$> dbField <$> snd <$> getEditable fields) <*> ZipList rs
                    setMessage "success"
                    -- redirect . groupR $ routes grid
                    defaultLayout $ [whamlet|hi|]
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost f
            done w e

-- lookup all items of a given type in the database
itemGrid :: ( PersistEntity p
            , PersistQuery (YesodPersistBackend m) (GHandler s m)
            , YesodPersist m
            , Bounded c
            , Enum c
            , PersistEntityBackend p ~ YesodPersistBackend m
            ) 
         => Grid s m p c
         -> Gridder s m p t
itemGrid g sel = do
    items <- runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    makeGrid g items sel

-- generate a grid showing the fields for items passed in, possibly including a form for editing a selected one
makeGrid :: ( Bounded c
            , Enum c
            , PersistEntityBackend p ~ YesodPersistBackend m
            ) 
         => Grid s m p c
         -> [Entity p]
         -> Gridder s m p t
makeGrid g items sel = return $ \extra -> do
            let cols = [minBound..maxBound]
                fields = getField g <$> cols
                disp (GridField _ extract display _) = display . extract . entityVal
                dispRow i = [whamlet|
$forall f <- fields
    <td>
        #{disp f i}
<td>
    <a href=@{(indR $ routes g) $ entityKey i}> edit  
|]
                widget = [whamlet|
^{extra}
<table>
    <thead>
        <tr>
            $forall c <- cols
                <th> #{(heading $ (getField g) c) c}
            <th> actions
    <tbody>
        $forall i <- items
            <tr>                       
                    ^{dispRow i}                                
|]
            return (undefined, widget)