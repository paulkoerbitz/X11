module Main

%lib C "m"
%lib C "X11"

sin' : Float -> IO Float
sin' x = mkForeign (FFun "sin" [FFloat] FFloat) x

openDisp : String -> IO Ptr
openDisp s = mkForeign (FFun "XOpenDisplay" [FString] FPtr) s

mkScreen : Ptr -> IO Int
mkScreen p = mkForeign (FFun "XDefaultScreen" [FPtr] FInt) p

whitePixel : Ptr -> Int -> IO Int
whitePixel dsp scrn = mkForeign (FFun "XWhitePixel" [FPtr, FInt] FInt) dsp scrn

blackPixel : Ptr -> Int -> IO Int
blackPixel dsp scrn = mkForeign (FFun "XBlackPixel" [FPtr, FInt] FInt) dsp scrn

main : IO ()
main = do
  dsp <- openDisp ""
  scrn <- mkScreen dsp
  bkgnd <- whitePixel dsp scrn
  frgnd <- blackPixel dsp scrn
  return ()
