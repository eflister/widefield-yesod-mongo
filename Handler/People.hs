module Handler.People
    where

import Import
import Text.Blaze
import Control.Applicative

personAgeForm :: (PersistStore (YesodPersistBackend master) (GHandler sub master), YesodPersist master) =>
    Key (YesodPersistBackend master) (PersonGeneric backend) {-PersonId-} -> GHandler sub master (Form Int)
personAgeForm pid = (renderDivs . areq intField "age?") <$> (personAge <$>) <$> (runDB $ get pid)

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
{-
            return (ageRes, widget)
                where widget = do [whamlet|
  #{extra}
  ^{fvInput ageView} -- ageView not in scope?
|]
-}

getPeopleR :: Handler RepHtml
getPeopleR = peopleGrid Nothing

getPersonR :: PersonId -> Handler RepHtml
getPersonR pid = do
    (w, e) <- generateFormPost =<< personAgeMForm pid
    peopleGrid $ Just (pid, w, e)

postPersonR :: PersonId -> Handler RepHtml
postPersonR pid = do
    ((r, w), e) <- runFormPost =<< personAgeMForm pid
    case r of
        FormSuccess age -> do
            runDB $ update pid [PersonAge =. age]
            setMessage "success"
            redirect PeopleR
        _               -> peopleGrid $ Just (pid, w, e) -- use contents of failure somehow?  does fvErrors already get it all?

peopleGrid :: Maybe (PersonId, Widget, Enctype) -> Handler RepHtml
peopleGrid sel = defaultLayout $ do
    people <- lift . runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> people
    makeGrid sel people . getZipList $ 
        GridField <$> ZipList ["name"    , "age"           ] 
                  <*> ZipList [personName, show . personAge] -- show shouldn't be necessary
                  <*> ZipList [False     , True            ]
       
data Show s => GridField a s =  
    GridField { label    :: Text 
              , extract  :: a -> s -- Show s => (a -> s) 
              , editable :: Bool
              }

makeGrid :: (Show s, Text.Blaze.ToMarkup s) =>
	Maybe (PersonId, Widget, Enctype) -> [Entity Person] -> [GridField Person s] -> Widget
makeGrid sel items fields = 
    let disp i f  = {- show $ -} extract f $ entityVal i
        dispRow i = [whamlet|
$forall f <- fields
    <td>
        #{disp i f}
<td>
    <a href=@{PersonR $ entityKey i}> edit  
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
                $maybe (key, w, e) <- sel
                    $if key == entityKey i
                        <form method=post action=@{PersonR key} enctype=#{e}>
                        $# this form tag closes immediately, can it not cross the table cells?
                            $forall f <- fields
                                <td>
                                    $if editable f
                                        ^{w}
                                    $else
                                        #{disp i f}
                            <td>
                                <input type="submit" value="save">
                                <a href=@{PeopleR}> 
                                    <input type="button" value="cancel">
                    $else
                        ^{dispRow i}
                $nothing
                    ^{dispRow i}                                
|]