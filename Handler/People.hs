module Handler.People where

import Import
import CRUDGrid
import qualified Data.Text as T

getPeopleR :: Handler RepHtml
getPeopleR = groupGet peopleGrid

getPersonR, postPersonR :: PersonId -> Handler RepHtml
getPersonR  = formGet  peopleGrid
postPersonR = formPost peopleGrid

data PersonColumn = Name | Age
   deriving (Eq, Show, Bounded, Enum)

peopleGrid = Grid "People" (Routes PersonR PeopleR) $ \c -> case c of 
    Name -> GridField show personName T.unpack . Just $ Editable textField PersonName id True
--  Name -> GridField show personName T.unpack Nothing
    Age  -> GridField show personAge  show     . Just $ Editable intField  PersonAge  id True