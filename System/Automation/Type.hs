module System.Automation.Type
  ( getCurrentFocus
  , freeWinRef
  , WindowReference
  , sendString
  , getWindowTitle
  ) where

import Data.Char
import Data.Maybe

import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Unsafe.Coerce

type KeyCombo = [(KeySym, KeyMask)]
type WindowReference = (Display, Window)

freeWinRef :: WindowReference -> IO ()
freeWinRef (display, window) = closeDisplay display

getCurrentFocus :: IO WindowReference
getCurrentFocus = do display <- openDisplay ":0"
                     (window, mode) <- getInputFocus display
                     return (display, window)

noModifier :: KeyMask
noModifier = 0

keysymTable :: [(Char, KeyCombo)]
keysymTable = [ (' ',  [(xK_space,        noModifier)] )
              , ('!',  [(xK_exclam,       shiftMask )] )
              , ('"',  [(xK_quotedbl,     shiftMask )] )
              , ('#',  [(xK_numbersign,   shiftMask )] )
              , ('$',  [(xK_dollar,       shiftMask )] )
              , ('%',  [(xK_percent,      shiftMask )] )
              , ('&',  [(xK_ampersand,    shiftMask )] )
              , ('\'', [(xK_apostrophe,   noModifier)] )
              , ('(',  [(xK_parenleft,    noModifier)] )
              , (')',  [(xK_parenright,   noModifier)] )
              , ('*',  [(xK_asterisk,     shiftMask )] )
              , ('+',  [(xK_plus,         shiftMask )] )
              , (',',  [(xK_comma,        noModifier)] )
              , ('-',  [(xK_minus,        noModifier)] )
              , ('.',  [(xK_period,       noModifier)] )
              , ('/',  [(xK_slash,        noModifier)] )
              , (':',  [(xK_colon,        shiftMask )] )
              , (';',  [(xK_semicolon,    noModifier)] )
              , ('<',  [(xK_less,         noModifier)] )
              , ('=',  [(xK_equal,        noModifier)] )
              , ('>',  [(xK_greater,      shiftMask )] )
              , ('?',  [(xK_question,     shiftMask )] )
              , ('@',  [(xK_at,           shiftMask )] )
              , ('[',  [(xK_bracketleft,  noModifier)] )
              , ('\\', [(xK_backslash,    noModifier)] )
              , (']',  [(xK_bracketright, noModifier)] )
              , ('_',  [(xK_underscore,   shiftMask )] )
              , ('`',  [(xK_grave,        noModifier)] )
              , ('^',  [(xK_asciicircum,  shiftMask )] )
              , ('{',  [(xK_braceleft,    shiftMask )] )
              , ('}',  [(xK_braceright,   shiftMask )] )
              , ('~',  [(xK_asciitilde,   shiftMask )] )
              , ('|',  [(xK_bar,          shiftMask )] )
              , ('\t', [(xK_Tab,          noModifier)] )
              , ('\r', [(xK_Return,       noModifier)] )
              , ('\n', [(xK_Return,       noModifier)] )
              ]

keysymLookup :: Char -> KeyCombo
keysymLookup c = fromMaybe defaultCombo specialValue
  where defaultCombo = [(stringToKeysym [c], upperMask c)]
        upperMask c = if isUpper c then shiftMask else noModifier
        specialValue = c `lookup` keysymTable

sendSingle :: WindowReference -> (KeySym, KeyMask) -> IO ()
sendSingle (display, window) (keysym, modifiers) = do
    keycode <- keysymToKeycode display keysym

    event <- mallocBytes 96
    pokeByteOff event 12 (unsafeCoerce display :: IntPtr) -- Display Pointer
    pokeByteOff event 16 window                           -- Recieving window
    pokeByteOff event 20 $ defaultRootWindow display      -- Root window
    pokeByteOff event 24 (0 :: Int)                       -- Subwindow (None)
    pokeByteOff event 32 (1 :: Int)                       -- X
    pokeByteOff event 36 (1 :: Int)                       -- Y
    pokeByteOff event 40 (1 :: Int)                       -- Root X
    pokeByteOff event 44 (1 :: Int)                       -- Root Y
    pokeByteOff event 48 modifiers                        -- Mod Keys
    pokeByteOff event 52 keycode                          -- Key Code
    pokeByteOff event 56 True                             -- Same Screen

    -- Send Key Press
    time <- gettimeofday_in_milliseconds
    pokeByteOff event 28 (fromInteger time :: Int)
    pokeByteOff event 00 keyPress
    sendEvent display window True keyPressMask (castPtr event)

    -- Send Key Release
    time <- gettimeofday_in_milliseconds
    pokeByteOff event 28 (fromInteger time :: Int)
    pokeByteOff event 00 keyRelease
    sendEvent display window True keyReleaseMask (castPtr event)
    free event

sendCombo :: WindowReference -> KeyCombo -> IO ()
sendCombo winRef keys = do mapM (sendSingle winRef) keys
                           return ()

sendCharacter :: WindowReference -> Char -> IO ()
sendCharacter winRef = sendCombo winRef . keysymLookup

sendString :: WindowReference -> String -> IO ()
sendString winRef text = do mapM (sendCharacter winRef) text
                            return ()

getWindowTitle :: WindowReference -> IO String
getWindowTitle (display, window) = do name <- fetchName display window
                                      return $ fromMaybe "" name
