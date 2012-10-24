{-# LANGUAGE RankNTypes #-}

module Handler.People
    where

import Import
import Text.Blaze
import Control.Applicative
import Data.Maybe

getPeopleR :: Handler RepHtml
getPeopleR = undefined

getPersonR :: PersonId -> Handler RepHtml
getPersonR = undefined -- gridForm False peopleGrid

postPersonR :: PersonId -> Handler RepHtml
postPersonR = undefined -- gridForm True peopleGrid

-- supply Person-specific details to gridForm
peopleGrid :: (ToWidget App App a)
           => Maybe PersonId 
           -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], a))
                               , (PersonId -> Route App)
                               , Route App
                               , [GridField]
                               )
peopleGrid = itemGrid PersonR PeopleR . getZipList $ GridField 
    <$> ZipList ["name"                                    , "age"                                  ] 
    <*> ZipList [personName                                , show . personAge                       ] -- make this line existential somehow?  Show s => [Person -> s]
--  <*> ZipList [Just $ Editable textField PersonName False, Just $ Editable intField PersonAge True]
    <*> ZipList [Nothing                                   , Just $ Editable intField PersonAge True] -- fields not returning same type can't be in same list :(
    <*> ZipList [False                                     , True                                   ]

data GridField = 
    GridField { label    :: Text
              , extract  :: Person -> String
              , editable :: Maybe Editable
              , needShow :: Bool
              }

data Editable =
	Editable { htmlField :: Field App App Int
             , dbField   :: EntityField Person Int
             , required  :: Bool
             }

-- lookup all items of a given type in the database
itemGrid :: ( ToWidget App App b
            )
         => (PersonId -> Route App)
         -> Route App
         -> [GridField]
         -> Maybe PersonId
         -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], b))
                             , (PersonId -> Route App)
                             , Route App
                             , [GridField])
itemGrid indR groupR fields sel = do
    let items = undefined
    -- items <- {- lift . -} runDB $ selectList [] []
    liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    let grid = makeGrid indR groupR sel items fields
    return (grid, indR, groupR, fields)

-- run a form, extract results, and update the database
gridForm :: ( ToWidget App App b
            ) --(RenderMessage master FormMessage)
         => Bool
         -> (Maybe PersonId -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], b))
                                                , (PersonId -> Route App)
                                                , Route App
                                                , [GridField]
                                                ))
         -> PersonId
         -> Handler RepHtml
gridForm post gridder pid = do
    (grid, indR, groupR, fields) <- gridder $ Just pid
    let done w e = defaultLayout [whamlet|
$# this form tag closes immediately, can it not cross other tags?
<form method=post action=@{indR pid} enctype=#{e}>
    ^{w}
|]   
    if post 
        then do
            ((r, w), e) <- runFormPost =<< grid -- will this give results for optional and untouched fields?  we depend on the order, so...
            case r of
                FormSuccess rs -> do
                    runDB $ update pid . getZipList $ ZipList ((=.) <$> dbField <$> mapMaybe editable fields) <*> ZipList rs
                    setMessage "success"
                    redirect groupR 
                _ -> done w e -- use contents of failure somehow?  does fvErrors already get it all?
        else do
            (w, e) <- generateFormPost =<< grid
            done w e
        
-- generate a grid showing the fields for items passed in, possibly including a form for editing a selected one
makeGrid :: (ToWidget App App b)
         => (PersonId -> Route App)
         -> Route App
         -> Maybe PersonId
         -> [Entity Person]
         -> [GridField]
         -> GHandler App App (Markup -> MForm App App (FormResult [Int], b))
makeGrid = undefined
{-
makeGrid indR groupR sel items fields = 
    let getWidget f w = fromJust $ lookup (label f) w -- what do if can't find?
        disp i f      = (if needShow f then show else id) $ extract f $ entityVal i
        dispRow i     = [whamlet|
$forall f <- fields
    <td>
        #{disp i f}
<td>
    <a href=@{indR $ entityKey i}> edit  
|]
    in [whamlet|
<table>
    <thead>
        <tr>
            $forall f <- fields
                <th> #{label f}
            <th> actions
    <tbody>
        $forall i <- items
            <tr>        
                $maybe (key, w, x, e) <- sel
                    $if key == entityKey i
                            ^{x}
                            $forall f <- fields
                                <td>
                                    $maybe _ <- editable f
                                        ^{getWidget f w}
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
-}

{-
personAgeMForm :: (PersistStore (YesodPersistBackend master) (GHandler sub master), YesodPersist master, RenderMessage master FormMessage) 
     => Key (YesodPersistBackend master) (PersonGeneric backend) -- PersonId
     -> GHandler sub master (Html -> MForm sub master (FormResult Int, GWidget sub master ()))
personAgeMForm pid = dMForm <$> (personAge <$>) <$> (runDB $ get pid)
    where dMForm mage extra = do
            (ageRes, ageView) <- mreq intField "unused" mage
            let widget = do [whamlet|
  $#adapted from renderDivs
  #{extra}
  <div :fvRequired ageView:.required :not $ fvRequired ageView:.optional>
        <label for=#{fvId ageView}>
            $maybe tt <- fvTooltip ageView
                <div .tooltip>#{tt}
            ^{fvInput ageView}
            $maybe err <- fvErrors ageView
                <div .errors style="color:red">#{err}
|]
            return (ageRes, widget)
-}