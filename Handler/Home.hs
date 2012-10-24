{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables, RankNTypes #-}
module Handler.Home -- (Subject, TrialRange, Analysis) -- trying to export these to Foundation.hs doesn't work...
    ( Subject
    , TrialRange
    , Analysis
    , getHomeR
    , postHomeR
    , getOverviewR
    , getImageR 
    ) where

import Import
import System.Directory
import System.FilePath
import Control.Monad
import System.Process -- some commands require that we run from mingw -- matlab ok in dos, dir isn't
import Prelude (head)
import Data.List.Split
import Data.Maybe
import Control.Shortcircuit hiding ((&&))
import Data.List hiding (insert)
import Data.Function
import Control.Arrow
import Data.Text.IO (readFile)
import qualified Data.Text as T

instance Show FileInfo

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler RepHtml
getHomeR = do
    (w, e) <- generateFormPost sampleForm
    content "getHomeR" w e Nothing
    
postHomeR :: Handler RepHtml
postHomeR = do
    ((result, w), e) <- runFormPost sampleForm
    -- liftIO . putStrLn $ show result -- causes stack overflow on FormSuccess, but not FormFailure?
    content "postHomeR" w e $ case result of
            FormSuccess res -> Just res
            _ -> Nothing

instance Shortcircuit ([] a)
    where isTrue = not . null                   

content (handlerName :: Text) 
        formWidget 
        formEnctype 
        submission = defaultLayout $ do
            aDomId <- lift newIdent
            setTitle "Welcome To Yesod!"
            stuff <- liftIO $ do
                let log = "log.txt"
                void $ rawSystem "matlab" ["-nodesktop", "-nosplash", "-wait", "-logfile", log, "-r", "x=2;x+2,quit"]
                lg <- readFile log
                return . splitOn "\n" $ T.unpack lg
            people <- lift . runDB $ do
                orM ( do liftIO $ putStrLn "checking"
                         (entityVal <$>) <$> (selectList [PersonName ==. "Stacy"] [])
                   )( do liftIO $ putStrLn "generated"
                         maybeToList <$> (get =<< (insert $ Person "Stacy" 26))
                    )

            [whamlet| hi #{personName =<< people}<p> |]
            $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing


{- /overview
 -}

-- for now these are duplicated in Foundation.hs, don't know how to export
type Subject    = String
type Analysis   = String
type TrialRange = String

analysisDir = "\\\\landis\\Users\\nlab\\Desktop\\analysis"
fileExt = ".png"

getOverviewR :: Handler RepHtml
getOverviewR = defaultLayout $ do
    setTitle "widefield"
    (subjTrials, files) <- liftIO $ do
        fn <- (splitExtensions . fst <$>) <$> filter ((== fileExt) . snd) <$> (splitExtension <$>) <$> getDirectoryContents analysisDir
        let tns  = splitExtensions . tail . snd <$> fn
            fs   = tail <$> (nub $ snd <$> tns)
            sts  = ((join (***) nub) . unzip <$>) <$> groupBy ((==) `on` fst) $ (zip `on` (fst <$>)) fn tns
            sts' = (head *** sortBy (compare `on` (read . takeWhile (/= '-') :: String -> Int))) <$> sts
        return (sts', fs)
    $(widgetFile "overview")

getImageR :: Subject -> Analysis -> TrialRange -> Handler RepPlain
getImageR sub ana rng = sendFile typePng $ analysisDir </> sub <.> rng <.> ana <.> (tail fileExt)