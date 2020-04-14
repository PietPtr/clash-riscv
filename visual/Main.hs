module Main where

import Clash.Prelude hiding (Text, map, (++), length, zip, take, (^))
import Core hiding (Tick)
import BaseTypes hiding (trace)
import Debug
import Fetch
import Decode
import Instructions

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
instrInfoPosP   = Point (10, 10.5)
instrInfoDim    = Point (1240, 10 + 2 * txth)
memoryPosP      = Point (10, y instrInfoDim + 10)
memoryDim       = Point (650, (32 + 1) * txth)
pcPosP          = memoryPosP |+| Point (x memoryDim, y memoryDim + txth)
-- without the +0.1 the memory isn't displayed correctly...
regPosP         = memoryPosP |+| Point (x memoryDim, txth + 0.1)
highlightBaseP  = memoryPosP |+| Point (24, 15)
controlsPosP    = memoryPosP |+| Point (0, y memoryDim + txth * 1)

memoryPos       = extr memoryPosP
pcPos           = extr pcPosP
regPos          = extr regPosP
controlsPos     = extr controlsPosP
instrInfoPos    = extr instrInfoPosP

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
           renderedMemory
        ++ renderedRegisters
        ++ renderedPC
        ++ renderControls
        ++ renderedInstrInfo
    where
        renderedMemory    = renderMemory $ memory $ state
        renderedRegisters = renderRegisters (registers state) (instruction)
        renderedPC        = renderPC $ pc $ state
        renderedInstrInfo = renderInstrInfo $ conv $ instructionData
        instruction = (decode . fetch . conv) instructionData
        instructionData = (memory state) Clash.Prelude.!! (pc state)


renderPC :: PC -> [Out] -- TODO: maak alsjeblieft van deze magic numbers constanten...
renderPC pc = numeric ++ highlighter
    where
        numeric         = onCanvas $ drawSF "pc" --klopt echt totaal niet lol
            $ txtShape pcPos ("                 pc" +-+ showHex ((conv pc) * 4) 8)
        highlighter     = onCanvas $ drawSF "pc-highlight" shape
        shape           = Rectangle pos (70, 18) nothing 1 red Nothing
        pos             = highlightBaseP |+| Point (75.7 * xmod, 20 * ymod)
        xmod :: Float   = conv (pc `mod` 8)
        ymod :: Float   = (fromIntegral . floor) $ (fromIntegral pc) / 8


renderRegisters :: RegisterBank -> Instruction -> [Out]
renderRegisters regs instr = onCanvas $ renderLines regPos $ formatRegisters regs instr

formatRegisters :: RegisterBank -> Instruction -> [String]
formatRegisters regs instr = formatted
    where
        formatted = mapi (\r i -> printf "%10s %-4s x%-2d %08x %-8i" (use i) (regnames i) i r (toSignedInt r)) listRegs'
        toSignedInt :: Integer -> Integer
        toSignedInt r = conv (conv r :: Signed 32)
        listRegs' :: [Integer] = map conv listRegs
        listRegs :: [Unsigned 32] = map conv (toList regs)
        rs1Index = rs1Is instr
        rs2Index = rs2Is instr
        rdIndex = rdIs instr
        use index =
            (if (Just index == rs1Index) then "rs1" else "") ++
            (if (Just index == rs2Index) then " rs2" else "") ++
            (if (Just index == rdIndex)  then " rd"   else "")

rs1Is :: Instruction -> Maybe Integer
rs1Is (RType _ rs1 _ _) = Just (conv rs1)
rs1Is (IType _ _ rs1 _) = Just (conv rs1)
rs1Is (SType _ _ rs1 _) = Just (conv rs1)
rs1Is _ = Nothing

rs2Is :: Instruction -> Maybe Integer
rs2Is (RType _ _ rs2 _) = Just (conv rs2)
rs2Is (SType _ _ _ rs2) = Just (conv rs2)
rs2Is _ = Nothing

rdIs :: Instruction -> Maybe Integer
rdIs (RType _ _ _ rd) = Just (conv rd)
rdIs (IType _ _ _ rd) = Just (conv rd)
rdIs (UType _ _ rd)   = Just (conv rd)
rdIs _ = Nothing

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


renderInstrInfo :: Unsigned 32 -> [Out]
renderInstrInfo instr = onCanvas $ renderLines instrInfoPos ["info", infoStr]
    where
        infoStr = printf "%60s" (decoded)
        decoded = (pretty . decode . fetch) instr


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
