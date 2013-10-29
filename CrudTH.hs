--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}

module CrudTH
    ( mkCRUD
    ) where

import Language.Haskell.TH.Syntax
import Prelude hiding ((++), take, concat, splitAt)
import Data.Monoid (mconcat, (<>))
import Database.Persist
import Control.Applicative
import Data.Text (unpack)
import Data.Char
import Yesod hiding (lift)
import Foundation
import Control.Monad
import CRUDGrid

mkCRUD :: (Lift b, Lift (JSGrid c)) => String -> b -> Maybe (JSGrid c) -> [EntityDef a {-SqlType-}] -> Q [Dec]
mkCRUD t d j = fmap mconcat . mapM (crudFromEntity t d j) . filter ((elem "CRUDGrid") . entityAttrs)

crudFromEntity :: (Lift b, Lift (JSGrid c)) => String -> b -> Maybe (JSGrid c) -> EntityDef a -> Q [Dec]
crudFromEntity t d j e = do
    reportWarning "crud"
    runIO $ do
        putStrLn $ show ex    
        putStrLn $ show ats
        putStrLn $ show fAts
--    n <- newName "crudTest"    

    when (any (notElem "Required") (fieldAttrs <$> entityFields e)) $ error "CRUDGrid: optional fields not implemented yet, mark all fields Required in model entities with CRUDGrid activated"

    d' <- runQ [| d |]
    j' <- runQ [| j |]

    return $ mconcat [ dec (e' False "get"     "s" True) "groupGet"    handlerHtml
                     , dec (mkR      "getNew"          ) "formNewGet"  handlerHtml
                     , dec (mkR      "postNew"         ) "formNewPost" handlerHtml

                     , dec (mkR "get"       ) "formGet"    idHandler
                     , dec (mkR "post"      ) "formPost"   idHandler
                     , dec (mkR "postDelete") "formDelete" idHandler

                     , [DataD [] (e' False "" "Column" False) [] columns [''Eq, ''Show, ''Bounded, ''Enum]]

                     , [ FunD gridF [Clause []
                                            ( NormalB $ AppE (AppE (AppE (AppE (AppE (AppE (AppE (ConE 'Grid) $ LitE $ StringL t) $ ListE []) $ fix $ elem "Deletable" $ entityAttrs e) d') routes) j') lambda
                                            )
                                            []
                                    ]
                       ]               
                     ]

{-
    sequence [ [| $n :: () |]
             , [| $n = () |]
             ]
-}
    where
      fix True  = ConE 'True -- how do we get rid of these?
      fix False = ConE 'False
      routes = AppE (AppE (AppE (AppE (ConE 'Routes) $ ConE $ mkR "") $ ConE $ e' False "" "s" True) $ ConE $ e' False "New" "" True) $ ConE (e' False "Delete" "" True) -- PersonR PeopleR NewPersonR DeletePersonR
      lambda = LamCaseE $ (\f -> Match (ConP (mkName $ setCase toUpper $ unpack $ unHaskellName $ fieldHaskell f) []) (NormalB $ gf f) []) <$> entityFields e -- eliminate the unpack/unhaskellname/fieldhaskell redundancy
      update f = LamE [VarP $ mkName "x", VarP $ mkName "y"] $ RecUpdE (VarE $ mkName "y") [(f, VarE $ mkName "x")]
      editable f = AppE (ConE 'Just) $ AppE (AppE (AppE (AppE (ConE 'Editable) $ VarE f') $ ConE $ fieldName f False) (ConE 'True)) $ update $ fieldName f True
                   where f' = mkName $ f <> "Field"                  
      fieldName f x  = e' x "" (setCase toUpper f) False
      fields = unpack . unHaskellName . fieldHaskell <$> entityFields e
      setCase f (x:xs) = (f x):xs
      gf f = AppE (AppE (VarE $ mkName ".")
                        (AppE (AppE (AppE (ConE 'GridField) 
                                          $ VarE $ mkName "show"
                                          ) 
                                    $ VarE $ fieldName f' True
                                    )
                              (AppE (ConE 'Left) 
                                    $ VarE $ mkName "show"
                                    )
                              )
                        )
                  $ if elem "Editable" $ fieldAttrs f then editable f' else ConE 'Nothing
             where f' = unpack $ unHaskellName $ fieldHaskell f
      columns = (`NormalC` []) . mkName . setCase toUpper <$> fields

      ex = entityExtra e        
      ats = [show . entityHaskell, show . entityAttrs] <*> [e]
      fAts = [show . fieldHaskell, show . fieldType, show . fieldAttrs] <*> entityFields e
--      n = mkName . unpack $ "crudTest" <> (unHaskellName $ entityHaskell e)
      e' lower pre post res = mkName . (if lower then (setCase toLower) else id) $ pre <> (unpack . unHaskellName $ entityHaskell e) <> post <> if res then "R" else ""
      gridF = e' True  "" "Grid" False
      mkR s = e' False s  ""     True
      handlerHtml = AppT (ConT ''Handler) (ConT ''Html)
      idHandler   = AppT (AppT ArrowT (ConT $ e' False "" "Id" False)) handlerHtml
      dec n b t = [ SigD n t
                  , FunD n [Clause [] 
                                   (NormalB $ AppE (VarE $ mkName b) 
                                                   (VarE   gridF)
                                   ) 
                                   []
                           ]
                  ]

{-

   [EntityDef
       (HaskellName (Database.Persist.TH.pack' "Person"))
       (DBName (Database.Persist.TH.pack' "person"))
       (DBName (Database.Persist.TH.pack' "id"))
       [Database.Persist.TH.pack' "People", -- entityAttrs :: [Attr] (Text)
        Database.Persist.TH.pack' "Deletable",
        Database.Persist.TH.pack' "Creatable",
        Database.Persist.TH.pack' "PlainHTML"]
       [FieldDef
          (HaskellName (Database.Persist.TH.pack' "name"))
          (DBName (Database.Persist.TH.pack' "name"))
          (FTTypeCon Nothing (Database.Persist.TH.pack' "Text"))
          (persistent-1.2.0.1:Database.Persist.Sql.Class.sqlType
             (Nothing :: Maybe Text))
          [Database.Persist.TH.pack' "Required", -- fieldAttrs :: [Attr] (Text)
           Database.Persist.TH.pack' "Editable"]
          True
          Nothing,
        FieldDef
          (HaskellName (Database.Persist.TH.pack' "age"))
          (DBName (Database.Persist.TH.pack' "age"))
          (FTTypeCon Nothing (Database.Persist.TH.pack' "Int"))
          (persistent-1.2.0.1:Database.Persist.Sql.Class.sqlType
             (Nothing :: Maybe Int))
          [Database.Persist.TH.pack' "Required",
           Database.Persist.TH.pack' "Editable"]
          True
          Nothing]
       []
       [Database.Persist.TH.pack' "Show"]
       (containers-0.5.0.0:Data.Map.Base.fromList [])
       False]

       -}
