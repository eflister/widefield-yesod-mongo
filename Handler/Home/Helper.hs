module Handler.Home.Helper where

import Import hiding (lift)
import CRUDGrid
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Data.Text
--import Text.QuasiText (for instance Lift Text)

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

instance Lift Person where
	lift (Person n a) = [| Person n a |]

	-- stolen from aeson (consider Text.QuasiText in package QuasiText)
instance Lift Text where
  lift txt = [| pack $(lift (unpack txt)) |]

instance Lift (JSGrid a) where
	lift j = undefined