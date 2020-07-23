import qualified Data.Map as M
import Data.Map (Map)
import           XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.BoringWindows
import XMonad.Layout.ResizableTile
import XMonad.Wallpaper
import XMonad.Actions.SwapWorkspaces
import qualified XMonad as X

main = do
  setRandomWallpaper ["$HOME/.config/wallpapers"]
  xmonad $ def {
    layoutHook =  boringWindows $ tall ||| Full
  , modMask = windowsMask -- altMask .|. controlMask
  , keys = \conf -> myKeys conf `M.union` keys def conf -- M.union is left biased
  , startupHook = do
      spawn "xmodmap ~/.Xmodmap"
  }
  where
    altMask = mod1Mask
    windowsMask = mod4Mask
    noMask = 0
    tall = ResizableTall {
      _nmaster = 1     -- number of master windows
    , _delta   = (3/100) -- change when resizing
    , _frac    = (1/2)    -- width of master
    , _slaves  = [ 2 ]  -- fraction to multiply to slave window heights
    }
    myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
    myKeys conf@(XConfig {
      X.modMask    = modm
    , X.workspaces = workspaces
    }) = M.fromList $ [
        ((modm, xK_r),     refresh)
      , ((modm, xK_d),     sendMessage Shrink)
      , ((modm, xK_minus), sendMessage Expand)
      , ((modm, xK_s),     sendMessage MirrorShrink)
      , ((modm, xK_h),     sendMessage MirrorExpand)
      ] ++ [
        ((modm .|. mask, xK_n), f)
          | (mask, f) <- [ (noMask, focusDown), (shiftMask, windows W.swapDown) ]
      ] ++ [
        ((modm .|. mask, xK_t), f)
          | (mask, f) <- [ (noMask, focusUp), (shiftMask, windows W.swapUp) ]
      ] ++ [
        ((modm .|. mask, key), screenWorkspace scrId >>= flip whenJust (windows . f))
          | (scrId, key) <- zip screens [xK_a, xK_o, xK_e, xK_u]
          , (mask, f)    <- [(noMask, W.view), (shiftMask, W.shift)]
      ] ++ [
        ((modm .|. mask, key), windows $ f wspId)
          | (wspId, key) <- zip workspaces [xK_1 ..]
          , (mask, f) <- [
              (noMask,      W.greedyView)
            , (shiftMask,   W.shift)
            , (controlMask, swapWithCurrent)
            ]
      ] ++ [
        ((modm, xK_0), withFocused $ windows . W.sink )
      ]
      where
        screens = [0..]
  
