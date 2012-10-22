module Handler.People
    where

import Import
import Text.Blaze
import Control.Applicative

personAgeForm :: (PersistStore (YesodPersistBackend master) (GHandler sub master), YesodPersist master) =>
    Key (YesodPersistBackend master) (PersonGeneric backend) {-PersonId-} -> GHandler sub master (Form Int)
personAgeForm id = (renderDivs . areq intField "age?") <$> (personAge <$>) <$> (runDB $ get id)

{-
-- here's where we are:  get this to compile
personAgeMForm :: (PersistStore (YesodPersistBackend App) (GHandler App App), YesodPersist App)
    => Key (YesodPersistBackend App) (PersonGeneric backend) -- PersonId
    -> Html 
    -> GHandler App App (MForm App App (FormResult Int, Widget))
personAgeMForm id extra = do
    (ageRes, ageView) <- (mreq intField "unused") <$> (personAge <$>) <$> (runDB $ get id)
    let widget = do [whamlet|
  #{extra}
  ^{fvInput ageView}
|]
    return (ageRes, widget)
-}

{-               
personMForm :: forall t (backend :: (* -> *) -> * -> *) sub master.
	RenderMessage master FormMessage =>
    t -> transformers-0.3.0.0:Control.Monad.Trans.RWS.Lazy.RWST
            (Maybe (Env, FileEnv), master, [Yesod.Form.Types.Lang])
            Enctype
            Ints
            (GHandler sub master)
            (FormResult (PersonGeneric backend), GWidget sub master ())
-}

personMForm :: Html -> MForm App App (FormResult Person, Widget)
personMForm extra = do
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (ageRes, ageView) <- mreq intField "neither is this" Nothing
    let personRes = Person <$> (show <$> nameRes) <*> ageRes
    let widget = do [whamlet|
  #{extra}
  ^{fvInput nameView}
  ^{fvInput ageView}
  <input type=submit value="Introduce myself">
|]
    return (personRes, widget)

getPeopleR :: Handler RepHtml
getPeopleR = peopleGrid Nothing

getPersonR :: PersonId -> Handler RepHtml
getPersonR id = do
    (w, e) <- generateFormPost =<< personAgeForm id
    peopleGrid $ Just (id, w, e)

postPersonR :: PersonId -> Handler RepHtml
postPersonR id = do
    ((r, w), e) <- runFormPost =<< personAgeForm id
    case r of
        FormSuccess age -> do
            runDB $ update id [PersonAge =. age]
            setMessage "success"
            redirect PeopleR
        _               -> peopleGrid $ Just (id, w, e) -- use contents of failure somehow?  they're already in w!

peopleGrid :: Maybe (PersonId, Widget, Enctype) -> Handler RepHtml
peopleGrid info = defaultLayout $ do
    people <- lift . runDB $ selectList [] []
    -- liftIO . mapM_ (putStrLn . show) $ entityKey <$> people
    makeGrid info people . getZipList $ 
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
makeGrid info items fields = [whamlet|
<table>
    <thead>
        <tr>
            $forall f <- fields
                <th> #{label f}
    <tbody>
        $forall i <- items
            <tr>        
                $forall f <- fields
                    <td>
                        $# --     show $ extract f $ entityVal i
                        $with nonEdit <- extract f $ entityVal i
                            $maybe (key, w, e) <- info
                                $if (&&) (editable f) $ key == entityKey i
                                    <form method=post action=@{PersonR key} enctype=#{e}>
                                        ^{w}
                                        <input type="submit" value="save">
                                    <a href=@{PeopleR}> 
                                        <input type="button" value="cancel">
                                $else
                                    #{nonEdit}
                            $nothing
                                #{nonEdit}
                                $if editable f
                                    <a href=@{PersonR $ entityKey i}> edit
|]