{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VM.Runtime where 

import VM.Interpreter
import Compiler.Compiler
import Env
import Parser.Lang.MLLGlobal
import Parser.Lib

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (.=), (%=))
import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)
import Data.Text (pack)
import qualified Data.Text.Zipper as Z
import Data.Maybe (fromMaybe)


data Name = EditorName | ResultName
    deriving (Eq, Ord, Show)

data AppState = AppState
    { _editor :: E.Editor T.Text Name
    , _result :: T.Text
    }

makeLenses ''AppState

initialState :: AppState
initialState = AppState
    { _editor = E.editor EditorName Nothing ""
    , _result = ""
    }

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) = do
    s <- use editor
    let code = T.unlines $ E.getEditContents s
    result .= T.pack (interpretResult $ T.unpack code)

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    editor %= E.applyEdit (Z.insertMany "    ")

handleEvent (VtyEvent ev) = do
    zoom editor $ E.handleEditorEvent (VtyEvent ev)
handleEvent _ = return ()

drawEditor :: AppState -> Widget Name
drawEditor s = borderWithLabel (str "codeBox") $ vBox
    [ E.renderEditor (txt . T.unlines) True $ s^.editor,
      hBorder,
      vLimit 1 $ hBox [ hCenter $ str "[Esc] quit", fill ' ', hCenter $ str "[Ctrl + x] execute code" ]
    ]

drawResult :: AppState -> Widget Name
drawResult s = borderWithLabel (str "Result") $ vLimit 1 $ hBox[str $ T.unpack $ s^.result, fill ' ']

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = vBox [drawEditor s, drawResult s]


app :: App AppState e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr []
    }

display :: IO Int
display = do
  _ <- defaultMain app initialState
  return 0

interpretResult :: String -> String
interpretResult input = case runParser parseMLLProgram input of
    Left err -> "Error: " <> (show err)
    Right (program, _) -> do
        let instrs = runCompiler program
        let (env, instrs') = analyseLabels instrs emptyEnvironment 0
        let (_, finalStack, _, _) = execute instrs' (env, [], [], fromMaybe (error "Label not found") (getLabel (pack "main") env))
        show finalStack