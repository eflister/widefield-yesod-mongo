{-# LANGUAGE RankNTypes #-}

module Handler.People
    where

import Import
import Text.Blaze
import Control.Applicative
import Data.Maybe
import Data.Traversable (sequenceA)
import Control.Monad
import Prelude (head)

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
    <*> ZipList [False                                     , False                                  ]

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
	Editable { uiField  :: Field App App Int
             , dbField  :: EntityField Person Int
             , required :: Bool
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
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> items
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
      
-- split these up into requireds and optionals so we can keep their form results/types separate
getEditable fs = (\x -> (x, fromJust $ editable x)) <$> filter (isJust . editable) fs

getDefaultedViews :: Maybe PersonId 
                  -> [Entity Person] 
                  -> [GridField] 
                  -> MForm App App ( FormResult [Int]
                                   , [(Text, FieldView App App)]
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
makeGrid :: ()
         => (PersonId -> Route App)
         -> Route App
         -> Maybe PersonId
         -> [Entity Person]
         -> [GridField]
         -> GHandler App App (Markup -> MForm App App (FormResult [Int], GWidget App App ()))
makeGrid indR groupR sel items fields = return $ \extra -> do
            (rs, vs) <- getDefaultedViews sel items fields
            let getView f = fromJust $ lookup (label f) vs -- what do if can't find?
                disp i f  = (if needShow f then show else id) $ extract f $ entityVal i
                dispRow i = [whamlet|
$forall f <- fields
    <td>
        #{disp i f}
<td>
    <a href=@{indR $ entityKey i}> edit  
|]
                widget = [whamlet|
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
                                                        <div .errors style="color:red">#{err}
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