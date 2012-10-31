module Handler.People where

import Import
import CRUDGrid
import qualified Data.Text as T

getPeopleR, getNewPersonR, postNewPersonR :: Handler RepHtml
getPeopleR     = groupGet    peopleGrid
getNewPersonR  = formNewGet  peopleGrid
postNewPersonR = formNewPost peopleGrid

getPersonR, postPersonR, getDeletePersonR :: PersonId -> Handler RepHtml
getPersonR        = formGet    peopleGrid
postPersonR       = formPost   peopleGrid
getDeletePersonR  = formDelete peopleGrid -- wanted a DELETE method on the PersonR route, but how specify method from web page?

data PersonColumn = Name | Age
   deriving (Eq, Show, Bounded, Enum)

peopleGrid :: Grid s App Person PersonColumn
peopleGrid = Grid "People" True (Just $ Person namePrompt 0) (Routes PersonR PeopleR NewPersonR DeletePersonR) $ \c -> case c of 
--  Name -> GridField show personName T.unpack Nothing
    Name -> GridField show personName T.unpack . Just $ Editable nameField PersonName True (\x p -> (\y -> y{personName = x}) <$> p)
    Age  -> GridField show personAge  show     . Just $ Editable ageField  PersonAge  True (\x p -> (\y -> y{personAge  = x}) <$> p)

ageField :: Integral a => Field s App a
ageField = checkBool (>= 0) ageMsg intField

nameField :: Field s App Text
nameField = checkBool (not . T.isInfixOf namePrompt) nameMsg textField

namePrompt, ageMsg, nameMsg :: Text
namePrompt = "<type name>" -- this doesn't work if contains <>
ageMsg     = "age must be >= 0"
nameMsg    = "name must not contain \"" `T.append` namePrompt `T.append` "\""