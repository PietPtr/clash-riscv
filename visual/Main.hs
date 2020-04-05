module Main where

import Clash.Prelude hiding (Text, map, (++), length, zip, take)
import Core hiding (Tick)
import BaseTypes hiding (trace)
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
nothing = (  0,   0,   0,   0)
red     = (255,   0,   0, 255)

canvasSize = (1240, 1024)

txth = 20
intsrInfoPosP   = Point (10, 10)
instrInfoDim    = Point (1240, 150)
memoryPosP      = Point (10, y instrInfoDim + 10)
memoryDim       = Point (690, (32 + 1) * txth)
pcPosP          = memoryPosP |+| Point (x memoryDim, y memoryDim + txth)
-- without the +0.1 the memory isn't displayed correctly...
regPosP         = memoryPosP |+| Point (x memoryDim, txth + 0.1)
highlightBaseP  = memoryPosP |+| Point (24, 15)
controlsPosP    = memoryPosP |+| Point (0, y memoryDim + txth * 1)

memoryPos       = extr memoryPosP
pcPos           = extr pcPosP
regPos          = extr regPosP
controlsPos     = extr controlsPosP

extr :: Point -> (X, Y)
extr (Point (x, y)) = (x, y)

drawSF name shape = [ Draw $ Stateful name 0 shape ]

txtShape (x,y) str = Text str "Courier" 14 (Point (x,y)) AlignLeft black 1 black Nothing
txtShapeR (x,y) str = (txtShape (x, y) str) { alignment = AlignRight }


defaultCanvasSetup = C.SetupCanvas canvasId 1 canvasSize (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))

onCanvas :: [StatefulGraphicsOut] -> [Out]
onCanvas graphics = map (\a -> OutStatefulGraphics 1 [a]) graphics


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
        , OutTimer (SetIntervalTimer "autorun" 80000)
        ] ++ renderCore (coreState state))

    InTimer (Tick "autorun") -> (state', out)
        where
            state' = state { coreState = coreState' }

            (coreState', out) = if autorunEnabled state
                then advanceAndRender (coreState state)
                else (coreState state, [])

    InKeyboard (K.Key "enter") -> (state', [])
        where state' = state { autorunEnabled = not (autorunEnabled state) }

    InKeyboard (K.Key "right") -> (state', out)
        where
            state' = state { coreState = coreState' }

            (coreState', out) = if autorunEnabled state
                then (coreState state, [])
                else advanceAndRender (coreState state)

    InKeyboard (K.Key "backspace") -> (state { coreState = initialState }, renderCore initialState)

    -- TODO: show autorun status
    -- TODO: render program counter correctly
    -- TODO: prehaps restructure such that a memory element is a single object and update only it?

    _ -> (state, [])

advanceAndRender :: SystemState -> (SystemState, [Out])
advanceAndRender state = (state', renderCore state)
    where
        state' = core state 0

renderCore :: SystemState -> [Out]
renderCore state =
        renderedMemory ++
        renderedRegisters ++
        renderedPC ++
        renderControls
    where
        renderedMemory    = renderMemory $ memory $ state
        renderedRegisters = renderRegisters $ registers $ state
        renderedPC        = renderPC $ pc $ state


renderPC :: PC -> [Out] -- TODO: maak alsjeblieft van deze magic numbers constanten...
renderPC pc = numeric ++ highlighter
    where
        numeric         = onCanvas $ drawSF "pc" --klopt echt totaal niet lol
            $ txtShape pcPos ("      pc" +-+ showHex (conv (pc * 4)) 8)
        highlighter     = onCanvas $ drawSF "pc-highlight" shape
        shape           = Rectangle pos (70, 18) nothing 1 red Nothing
        pos             = highlightBaseP |+| Point (75.7 * xmod, 20 * ymod)
        xmod :: Float   = conv (pc `mod` 8)
        ymod :: Float   = (fromIntegral . floor) $ (fromIntegral pc) / 8


renderRegisters :: RegisterBank -> [Out]
renderRegisters regs = onCanvas $ renderLines regPos $ formatRegisters $ regs

formatRegisters :: RegisterBank -> [String]
formatRegisters regs = formatted
    where
        formatted = mapi (\r i -> printf "%s x%-2d %08x" (regnames i) i r) listRegs'
        listRegs' :: [Integer] = map conv listRegs
        listRegs :: [Unsigned 32] = map conv (toList regs)


renderMemory :: Memory -> [Out]
renderMemory memory = onCanvas $ renderLines memoryPos $ formatMemory $ memory

formatMemoryList :: [Integer] -> [String]
formatMemoryList mem = header : (mapi (\chunk i -> formatLine chunk i) chunks)
    where
        header = "         " ++ (intercalate "       " (map (\a -> showHex a 2) [0,4..0x1c]))
        chunks = chunksOf 8 mem
        formatLine words index =
            (take 2 (showHex (index * 32) 3)) +-+
            (intercalate " " $ map (\a -> printf "%08x" (a)) words)

formatMemory :: Memory -> [String]
formatMemory mem = formatMemoryList listInts
    where
        listInts     :: [Integer]     = map (conv) listUnsigned
        listUnsigned :: [Unsigned 32] = map (conv) (toList mem)


renderControls :: [Out]
renderControls = onCanvas $ renderLines controlsPos lines
    where
        lines = map (\(k, e) -> printf "%12s: %s" k e) keyExplanation
        keyExplanation =
            [ ("Backspace", "Reset the simulation")
            , ("Enter",     "Toggle automatic running")
            , ("Right",     "Advance one step") ]

showHex :: Integer -> Integer -> String
showHex n d = printf ("%0" ++ (show d) ++ "x") n

renderLines :: (X, Y) -> [String] -> [StatefulGraphicsOut]
renderLines _ [] = []
renderLines (x, y) (line:lines) = (drawSF ("name" ++ (show y)) shape) ++ renderLines (x, y + 20) lines
    where shape = txtShape (x, y) line


mapi :: (a -> Integer -> b) -> [a] -> [b]
mapi f as = map (\(a, i) -> f a i) combined
    where combined = zip as [0..]

a +-+ b = a ++ " " ++ b

regnames :: Integer -> String
regnames reg = case reg of
    0  -> "zero"
    1  -> "ra  "
    2  -> "sp  "
    3  -> "gp  "
    4  -> "tp  "
    5  -> "t0  "
    6  -> "t1  "
    7  -> "t2  "
    8  -> "fp  "
    9  -> "s1  "
    10 -> "a0  "
    11 -> "a1  "
    12 -> "a2  "
    13 -> "a3  "
    14 -> "a4  "
    15 -> "a5  "
    16 -> "a6  "
    17 -> "a7  "
    18 -> "s2  "
    19 -> "s3  "
    20 -> "s4  "
    21 -> "s5  "
    22 -> "s6  "
    23 -> "s7  "
    24 -> "s8  "
    25 -> "s9  "
    26 -> "s10 "
    27 -> "s11 "
    28 -> "t3  "
    29 -> "t4  "
    30 -> "t5  "
    31 -> "t6  "
