module Main where

import Clash.Prelude hiding (Text, map, (++), length, zip, take)
import Core hiding (Tick)
import BaseTypes
import Debug

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Vectors

import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.Websocket.Keyboard as K
import qualified Eventloop.Module.Websocket.Mouse as M
import Eventloop.Module.BasicShapes
import Eventloop.Module.StatefulGraphics
import Eventloop.Module.StdOut
import Eventloop.Module.Timer

import Debug.Trace
import Data.Maybe
import Data.List
import Data.List.Split

import Text.Printf

canvasId :: C.CanvasId
canvasId = 1

white   = (235, 228, 211, 255)
black   = ( 99,  97,  90, 255)

canvasSize = (1240, 1024)

drawSF name shape = [ Draw $ Stateful name 0 shape ]

txtShape (x,y) str = Text str "Courier" 14 (Point (x,y)) AlignLeft black 1 black Nothing

defaultCanvasSetup = C.SetupCanvas canvasId 1 canvasSize (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))


clearScreen = OutStatefulGraphics 1 (drawSF "clear" shape)
    where shape = Rectangle (Point (0, 0)) canvasSize white 0 white Nothing

data EventLoopState = EventLoopState
    { coreState      :: SystemState
    , autorunEnabled :: Bool
    }

eventloopStartState = EventLoopState
    { coreState      = initialState
    , autorunEnabled = False
    }

eventloopConfiguration = EventloopSetupConfiguration
    { beginProgstate            = eventloopStartState
    , eventloopF                = eventloop
    , setupModuleConfigurations =
        [ setupStatefulGraphicsModuleConfiguration
        , setupBasicShapesModuleConfiguration
        , C.setupCanvasModuleConfiguration
        , K.setupKeyboardModuleConfiguration
        , M.setupMouseModuleConfiguration
        , setupTimerModuleConfiguration
        , setupStdOutModuleConfiguration
        ]
    }

main :: IO ()
main = startEventloopSystem eventloopConfiguration

eventloop :: EventLoopState -> In -> (EventLoopState, [Out])
eventloop state event = case event of
    Start -> (state,
        [ OutCanvas  defaultCanvasSetup
        -- , clearScreen -- causes flickering :( also maybe unnecessary?
        , OutTimer (SetIntervalTimer "autorun" 1600)
        ])

    InTimer (Tick "autorun") -> (state',
        map (\a -> OutStatefulGraphics 1 [a]) graphics)
        where
            graphics = renderLines (10, 10) $ formatMemory $ memory $ coreState state
            state' = state { coreState = coreState' }
            coreState' = core (coreState state) 0

    -- TODO: respect autorun bool
    -- TODO: update autorun bool
    -- TODO: add key events for next previous
    -- TODO: render program counter
    -- TODO: prehaps restructure such that a memory element is a single object and update only it?

    _ -> (state, [])

renderLines :: (X, Y) -> [String] -> [StatefulGraphicsOut]
renderLines _ [] = []
renderLines (x, y) (line:lines) = (drawSF ("name" ++ (show y)) shape) ++ renderLines (x, y + 20) lines
    where shape = txtShape (x, y) line


formatMemoryList :: [Integer] -> [String]
formatMemoryList mem = header : (mapi (\chunk i -> formatLine chunk i) chunks)
    where
        header = "         " ++ (intercalate "       " (map (\a -> showHex a 2) [0,4..0x1c]))
        chunks = chunksOf 8 mem
        formatLine words index =
            (take 2 (showHex (index * 32) 3)) ++ " " ++
            (intercalate " " $ map (\a -> printf "%08x" (a)) words)

showHex :: Integer -> Integer -> String
showHex n d = printf ("%0" ++ (show d) ++ "x") n

formatMemory :: Memory -> [String]
formatMemory mem = formatMemoryList listInts
    where
        listInts     :: [Integer]     = map (conv) listUnsigned
        listUnsigned :: [Unsigned 32] = map (conv) (toList mem)

mapi :: (a -> Integer -> b) -> [a] -> [b]
mapi f as = map (\(a, i) -> f a i) combined
    where combined = zip as [0..]
