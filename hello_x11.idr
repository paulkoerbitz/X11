module Main

import Data.Bits as Bits

%include C "idris_x11.h"
%link C "idris_x11.o"

%lib C "m"
%lib C "X11"

sin' : Float -> IO Float
sin' x = mkForeign (FFun "sin" [FFloat] FFloat) x

openDisp : String -> IO Ptr
openDisp s = mkForeign (FFun "XOpenDisplay" [FString] FPtr) s

mkScreen : Ptr -> IO Int
mkScreen p = mkForeign (FFun "XDefaultScreen" [FPtr] FInt) p

rootWindow : Ptr -> Int -> IO Int
rootWindow dsp scrn = mkForeign (FFun "XRootWindow" [FPtr, FInt] FInt) dsp scrn

whitePixel : Ptr -> Int -> IO Int
whitePixel dsp scrn = mkForeign (FFun "XWhitePixel" [FPtr, FInt] FInt) dsp scrn

blackPixel : Ptr -> Int -> IO Int
blackPixel dsp scrn = mkForeign (FFun "XBlackPixel" [FPtr, FInt] FInt) dsp scrn

createGC : Ptr -> Int -> IO Ptr
createGC dsp wdw = mkForeign (FFun "XCreateGC" [FPtr, FInt, FInt, FPtr] FPtr) dsp wdw 0 prim__null

createWindow : Ptr -> Int -> Int -> Int -> IO Int
createWindow dsp scrn fgnd bgnd = do
  rwdw <- rootWindow dsp scrn
  mkForeign (FFun "XCreateSimpleWindow" [FPtr, FInt,
                                         FInt, FInt, FInt, FInt,
                                         FInt, FInt, FInt] FInt) dsp rwdw 0 0 200 200 5 fgnd bgnd

setBackground : Ptr -> Ptr -> Int -> IO ()
setBackground dsp gc bgnd = mkForeign (FFun "XSetBackground" [FPtr, FPtr, FInt] FUnit) dsp gc bgnd

setForeground : Ptr -> Ptr -> Int -> IO ()
setForeground dsp gc fgnd = mkForeign (FFun "XSetForeground" [FPtr, FPtr, FInt] FUnit) dsp gc fgnd

freeGc : Ptr -> Ptr -> IO ()
freeGc dsp gc = mkForeign (FFun "XFreeGC" [FPtr, FPtr] FUnit) dsp gc

destroyWindow : Ptr -> Int -> IO ()
destroyWindow dsp wdw = mkForeign (FFun "XDestroyWindow" [FPtr, FInt] FUnit) dsp wdw

closeDisplay : Ptr -> IO ()
closeDisplay dsp = mkForeign (FFun "XCloseDisplay" [FPtr] FUnit) dsp

data xK_Key =
    xK_q
  | xK_other

data XEvent =
    Expose Int Ptr Int
  | MappingNotify
  | ButtonPress Ptr Int Int Int
  | KeyPress xK_Key
  | OtherXEvent

malloc : Int -> IO Ptr
malloc sz = mkForeign (FFun "malloc" [FInt] FPtr) sz

free : Ptr -> IO ()
free p = mkForeign (FFun "free" [FPtr] FUnit) p

alloc : Int -> (Ptr -> IO a) -> IO a
alloc sz action = do
  ptr <- malloc sz
  res <- action ptr
  free ptr
  return res

nextXEvent : Ptr -> IO XEvent
-- FIXME: Size of XEvent here
nextXEvent dsp = alloc 1024 $ \evptr => (do
  xNextEvent dsp evptr
  evType <- getEventType evptr
  print evType
  evTypeCase evptr evType)
  where
    xNextEvent : Ptr -> Ptr -> IO ()
    xNextEvent dsp evptr = mkForeign (FFun "XNextEvent" [FPtr,FPtr] FUnit) dsp evptr

    getEventType : Ptr -> IO Int
    getEventType evptr = mkForeign (FFun "idr_getEventType" [FPtr] FInt) evptr

    getXExposeCount : Ptr -> IO Int
    getXExposeCount evptr = mkForeign (FFun "idr_getXExposeCount" [FPtr] FInt) evptr

    getXExposeDisplay : Ptr -> IO Ptr
    getXExposeDisplay evptr = mkForeign (FFun "idr_getXExposeDisplay" [FPtr] FPtr) evptr

    getXExposeWindow : Ptr -> IO Int
    getXExposeWindow evptr = mkForeign (FFun "idr_getXExposeWindow" [FPtr] FInt) evptr

    getXButtonDisplay : Ptr -> IO Ptr
    getXButtonDisplay evptr = mkForeign (FFun "idr_getXButtonDisplay" [FPtr] FPtr) evptr

    getXButtonWindow : Ptr -> IO Int
    getXButtonWindow evptr = mkForeign (FFun "idr_getXButtonWindow" [FPtr] FInt) evptr

    getXButtonX : Ptr -> IO Int
    getXButtonX evptr = mkForeign (FFun "idr_getXButtonX" [FPtr] FInt) evptr

    getXButtonY : Ptr -> IO Int
    getXButtonY evptr = mkForeign (FFun "idr_getXButtonY" [FPtr] FInt) evptr

    evTypeCase : Ptr -> Int -> IO XEvent
    evTypeCase evptr 12 = do
      cnt <- getXExposeCount evptr
      xdsp <- getXExposeDisplay evptr
      xwdw <- getXExposeWindow evptr
      return $ Expose cnt xdsp xwdw
    evTypeCase evptr 34 = return MappingNotify
    evTypeCase evptr 4 = do
      xdsp <- getXButtonDisplay evptr
      xwdw <- getXButtonWindow evptr
      x <- getXButtonX evptr
      y <- getXButtonY evptr
      return $ ButtonPress xdsp xwdw x y
    evTypeCase evptr 2 = return $ KeyPress xK_q
    evTypeCase evptr _ = return OtherXEvent


drawImageString : Ptr -> Int -> Ptr -> Int -> Int -> String -> IO ()
drawImageString dsp wdw gc x y s =
  mkForeign (FFun "XDrawImageString" [FPtr,FInt,FPtr,FInt,FInt,FString,FInt] FUnit) dsp wdw gc x y s (cast $ length s)

whileM : IO Bool -> IO ()
whileM action = action >>= \p => if p then whileM action else return ()

xSelectInput : Ptr -> Int -> IO ()
xSelectInput dsp wdw = mkForeign (FFun "XSelectInput" [FPtr,FInt,FInt] FUnit) dsp wdw 32773 -- (bitsToInt (ButtonPressMask `or` KeyPressMask `or` ExposureMask))
  -- where
  --   KeyPressMask    = 1 `shiftLeft`  0
  --   ButtonPressMask = 1 `shiftLeft`  2
  --   ExposureMask    = 1 `shiftLeft` 15

xMapRaised : Ptr -> Int -> IO ()
xMapRaised dsp wdw = mkForeign (FFun "XMapRaised" [FPtr,FInt] FUnit) dsp wdw

main : IO ()
main = do
  dsp <- openDisp ""
  scrn <- mkScreen dsp
  bkgnd <- whitePixel dsp scrn
  frgnd <- blackPixel dsp scrn
  wdw <- createWindow dsp scrn frgnd bkgnd
  -- Do we need XSetStandardProperties here -- I don't really know how to do this..
  -- Doesn't appear to be needed, at least the c program runs without it
  gc <- createGC dsp wdw
  setBackground dsp gc bkgnd
  setForeground dsp gc frgnd
  xSelectInput dsp wdw
  xMapRaised dsp wdw
  whileM $ do
    ev <- nextXEvent dsp
    case ev of
      (Expose cnt xdsp xwdw) => drawImageString xdsp xwdw gc 50 50 "Hello, World!" $> return True
      (MappingNotify)        => return True -- refreshKeyboardMapping ev >> return True
      (ButtonPress xdsp xwdw x y)      => print (x,y) $> drawImageString xdsp xwdw gc x y "Hi" $> return True
      (KeyPress xK_q)        => return False
      _ => return True
  freeGc dsp gc
  destroyWindow dsp wdw
  closeDisplay dsp
  return ()
