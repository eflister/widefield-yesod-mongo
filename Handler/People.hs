module Handler.People where

import Import
import CRUDGrid
import qualified Data.Text as T

getPeopleR :: Handler RepHtml
getPeopleR = groupGet peopleGrid

getPersonR, postPersonR :: PersonId -> GHandler s App RepHtml
getPersonR  = formGet  peopleGrid
postPersonR = formPost peopleGrid

data PersonColumn = Name | Age 
   deriving (Eq, Ord, Show, Bounded, Enum)

peopleGrid = itemGrid $ Grid (Routes PersonR PeopleR) $ \c -> case c of 
    Name -> GridField show personName T.unpack . Just $ Editable textField PersonName Right id False
    Age  -> GridField show personAge  show     . Just $ Editable intField  PersonAge  Right id True