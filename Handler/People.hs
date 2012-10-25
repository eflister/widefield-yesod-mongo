{-# LANGUAGE RankNTypes #-}

module Handler.People
    where

import Import
import Text.Blaze
import Control.Applicative
import Data.Maybe
import Data.Traversable

getPeopleR :: Handler RepHtml
getPeopleR = do
    (grid, _, _, _) <- peopleGrid Nothing
    defaultLayout . fst =<< generateFormPost =<< grid

getPersonR :: PersonId -> Handler RepHtml
getPersonR = gridForm False peopleGrid

postPersonR :: PersonId -> Handler RepHtml
postPersonR = gridForm True peopleGrid

-- supply Person-specific details to gridForm
peopleGrid :: ()
           => Maybe PersonId 
           -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], GWidget App App ()))
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

----------------------------------------------------------------------
-- everything below here designed to be abstractable from Person
----------------------------------------------------------------------

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
itemGrid :: ()
         => (PersonId -> Route App)
         -> Route App
         -> [GridField]
         -> Maybe PersonId
         -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], GWidget App App ()))
                             , (PersonId -> Route App)
                             , Route App
                             , [GridField])
itemGrid indR groupR fields sel = do
    items <- runDB $ selectList [] []
    liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
    let grid = makeGrid indR groupR sel items fields
    return (grid, indR, groupR, fields)

-- run a form, extract results, and update the database
gridForm :: ()
         => Bool
         -> (Maybe PersonId -> GHandler App App ( GHandler App App (Markup -> MForm App App (FormResult [Int], GWidget App App ()))
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
        
getDefaults :: Maybe PersonId 
            -> [Entity Person] 
            -> [GridField] 
            -> GHandler App App [Maybe Int]
getDefaults sel items fields = return []

getDefaultedWidgets :: [Maybe Int] 
                    -> [GridField] 
                    -> MForm App App ( FormResult [Int]
                                     , [(Text, GWidget App App ())]
                                     )
getDefaultedWidgets mDefaults fields = do
    pairs <- mreq intField "unused" <$> mDefaults
    return (sequenceA $ fst <$> pairs, [])
{-
getDefaultedWidgets mDefaults fields = do
    (rs, ws) <- mreq intField "unused" <$> mDefaults
    return (sequenceA rs, [])
-}

-- generate a grid showing the fields for items passed in, possibly including a form for editing a selected one
makeGrid :: ()
         => (PersonId -> Route App)
         -> Route App
         -> Maybe PersonId
         -> [Entity Person]
         -> [GridField]
         -> GHandler App App (Markup -> MForm App App (FormResult [Int], GWidget App App ()))
makeGrid indR groupR sel items fields = dMForm <$> getDefaults sel items fields
    where dMForm mDefaults extra = do
            (rs, ws) <- getDefaultedWidgets mDefaults fields
            let getWidget f = fromJust $ lookup (label f) ws -- what do if can't find?
                disp i f    = (if needShow f then show else id) $ extract f $ entityVal i
                dispRow i   = [whamlet|
$forall f <- fields
    <td>
        #{disp i f}
<td>
    <a href=@{indR $ entityKey i}> edit  
|]
                widget = [whamlet|
<table>
    <thead>
        <tr>
            $forall f <- fields
                <th> #{label f}
            <th> actions
    <tbody>
        $forall i <- items
            <tr>        
                ^{dispRow i} 
|]
            return (rs, widget)

{-
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

personAgeMForm :: (PersistStore (YesodPersistBackend master) (GHandler sub master), YesodPersist master, RenderMessage master FormMessage) 
     => Key (YesodPersistBackend master) (PersonGeneric backend) -- PersonId
     -> GHandler sub master (Html -> MForm sub master (FormResult Int, GWidget sub master ()))
personAgeMForm pid = dMForm <$> (personAge <$>) <$> (runDB $ get pid)
    where dMForm mage extra = do
            (ageRes, ageView) <- mreq intField "unused" mage
            let widget = [whamlet|
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