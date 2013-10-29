{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import CRUDGrid
import qualified Data.Text as T

import Database.Persist.Quasi
import CrudTH
import Handler.Home.Helper

{-
getPeopleR, getNewPersonR, postNewPersonR :: Handler Html
getPeopleR     = groupGet    peopleGrid
getNewPersonR  = formNewGet  peopleGrid
postNewPersonR = formNewPost peopleGrid

getPersonR, postPersonR, postDeletePersonR :: PersonId -> Handler Html
getPersonR        = formGet    peopleGrid
postPersonR       = formPost   peopleGrid
postDeletePersonR = formDelete peopleGrid -- wanted a DELETE method on the PersonR route, but how specify method from web page?
-}

{-
data PersonColumn = Name | Age
   deriving (Eq, Show, Bounded, Enum)
-}

{-
peopleGrid :: Grid App Person PersonColumn
peopleGrid = Grid "People" [] True (Just $ Person namePrompt 0) (Routes PersonR PeopleR NewPersonR DeletePersonR) Nothing {- (Just djdg) -} $ \c -> case c of 
--  Name -> GridField show personName (Left T.unpack)   Nothing
    Name -> GridField show personName (Left T.unpack) . Just $ Editable nameField PersonName True (\x y -> y{personName = x})
    Age  -> GridField show personAge  (Left show    ) . Just $ Editable ageField  PersonAge  True (\x y -> y{personAge  = x})
-}

{-
ageField :: ( Integral a
            , RenderMessage (HandlerSite s) FormMessage
            , Monad s
            ) 
         => Field s a
ageField = checkBool (>= 0) ageMsg intField

nameField :: ( RenderMessage (HandlerSite s) FormMessage
             , Monad s
             ) 
          => Field s Text
nameField = checkBool (not . T.isInfixOf namePrompt) nameMsg textField

namePrompt, ageMsg, nameMsg :: Text
namePrompt = "<type name>"
ageMsg     = "age must be >= 0"
nameMsg    = "name must not contain \"" `T.append` namePrompt `T.append` "\""
-}

--how make args to mkCRUD declarative in model rather than args passed in?  (note, these are arbitrary values, not preset flags like other attributes)
share [mkCRUD "People" (Just $ Person namePrompt 0) Nothing] $(persistFileWith lowerCaseSettings "config/models") -- can't do this in Model.hs cuz it cycles with CRUDGrid.hs

{-
-- download jqgrid and unpack to static path: http://www.trirand.com/blog/?page_id=6
-- also make and download a theme: http://jqueryui.com/themeroller/
jqg = JQGrid $ join (***) 
{-
        (StaticR <$>) -- have to cabal clean to pick these up, but they're compile-time verified
        ( [ jquery_ui_1_9_1_custom_css_ui_darkness_jquery_ui_1_9_1_custom_min_css
          , jquery_jqGrid_4_4_1_css_ui_jqgrid_css
          ]
        , [ jquery_jqGrid_4_4_1_js_i18n_grid_locale_en_js
          , jquery_jqGrid_4_4_1_js_jquery_jqGrid_min_js
          ]
        )
-}
        (makeStatic <$>) 
        ( [ "jquery-ui-1.9.1.custom/css/ui-darkness/jquery-ui-1.9.1.custom.min.css" 
          , "jquery.jqGrid-4.4.1/css/ui.jqgrid.css"
          ]
        , [ "jquery.jqGrid-4.4.1/js/i18n/grid.locale-en.js"
          , "jquery.jqGrid-4.4.1/js/jquery.jqGrid.min.js"
          ]
        )

-- installation: https://github.com/SitePen/dgrid/blob/master/README.md
djdg = DGrid (makeStatic "cpm_packages/dojo/dojo.js") [("data-dojo-config","async: true")]
               -- StaticR cpm_packages_dojo_dojo_js
-}