{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main, St(..), focusRing, iban, status) where

import qualified Brick.AttrMap        as A
import qualified Brick.Focus          as F
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import           Brick.Util           (on)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (hLimit, str, (<+>), (<=>))
import qualified Brick.Widgets.Edit   as E
import qualified Graphics.Vty         as V
import           Lens.Micro
import           Lens.Micro.TH        (makeLenses)
import           Lens.Micro.Mtl

data Name = Iban deriving (Ord, Show, Eq)

data St = St { _focusRing :: F.FocusRing Name
             , _iban      :: E.Editor String Name
             , _status    :: String
             }

makeLenses ''St

drawUi :: St -> [T.Widget Name]
drawUi st = [ui]
        where
            ibane = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.iban)
            ui = C.center $
                 (str "IBAN: " <+> hLimit 30 ibane) <=> str " "

initialState :: St
initialState =
        St (F.focusRing [Iban])
           (E.editor Iban (Just 1) "")
           ""

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent ev = do
        zoom iban $ E.handleEditorEvent ev

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
        [ ( E.editAttr, V.white `on` V.blue)
        , ( E.editFocusedAttr, V.black `on` V.yellow)
        ]

app :: M.App St e Name
app = M.App { M.appDraw = drawUi
            , M.appStartEvent = return ()
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const theMap
            , M.appChooseCursor = M.neverShowCursor
            }

main :: IO ()
main = do
        st <- M.defaultMain app initialState
        putStr "IBAN: "
        putStrLn $ unlines $ E.getEditContents $ st^.iban
