module Handler.People
    where

import Import
import Control.Applicative
import GridField

getPeopleR :: GHandler s App RepHtml
getPeopleR = groupGet peopleGrid

getPersonR, postPersonR :: PersonId -> GHandler s App RepHtml
getPersonR  = formGet  peopleGrid
postPersonR = formPost peopleGrid

peopleGrid :: Gridder s App Int Person
peopleGrid = itemGrid PersonR PeopleR . getZipList $ GridField 
    <$> ZipList ["name"                                    , "age"                                  ] 
    <*> ZipList [personName                                , show . personAge                       ] -- make this line existential somehow?  Show s => [Person -> s]
--  <*> ZipList [Just $ Editable textField PersonName False, Just $ Editable intField PersonAge True]
    <*> ZipList [Nothing                                   , Just $ Editable intField PersonAge True] -- fields not returning same type can't be in same list :(
    <*> ZipList [False                                     , False                                  ]