module Handler.People where

import Import
import CRUDGrid
import qualified Data.Text as T

getPeopleR, getNewPersonR, postNewPersonR :: Handler RepHtml
getPeopleR     = groupGet    peopleGrid
getNewPersonR  = formNewGet  peopleGrid
postNewPersonR = formNewPost peopleGrid

getPersonR, postPersonR, deletePersonR :: PersonId -> Handler RepHtml
getPersonR    = formGet    peopleGrid
postPersonR   = formPost   peopleGrid
deletePersonR = formDelete peopleGrid

data PersonColumn = Name | Age
   deriving (Eq, Show, Bounded, Enum)

peopleGrid = Grid "People" True (Just . Just $ Person "<type name>" 0) (Routes PersonR PeopleR newPersonR deletePersonR) $ \c -> case c of 
    Name -> GridField show personName T.unpack . Just $ Editable textField PersonName True (\x p -> (\y -> y{personName = x}) <$> p)
--  Name -> GridField show personName T.unpack Nothing
    Age  -> GridField show personAge  show     . Just $ Editable ageField  PersonAge  True (\x p -> (\y -> y{personAge  = x}) <$> p)

ageField = checkBool (>= 0) "ages >= 0" intField