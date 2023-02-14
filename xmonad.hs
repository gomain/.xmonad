import qualified Data.Map as Map
import Data.Map (Map)
import           XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.BoringWindows
import XMonad.Layout.ResizableTile
--import XMonad.Wallpaper
import XMonad.Actions.SwapWorkspaces
import qualified XMonad as X

main = do
  --setRandomWallpaper ["$HOME/.config/wallpapers"]
  xmonad $ def {
    layoutHook =  boringWindows $ tall ||| Full
  , modMask = windowsMask -- altMask .|. controlMask
  , keys = myKeys
  , startupHook = do
      spawn "xmodmap ~/.config/xmodmap/all_keyboards"
  }
  where
    altMask = mod1Mask
    windowsMask = mod4Mask
    noMask = 0
    tall = ResizableTall {
      _nmaster = 1       -- number of master windows
    , _delta   = (3/100) -- change when resizing
    , _frac    = (1/2)   -- width of master
    , _slaves  = [ 2 ]   -- fraction to multiply to slave window heights
    }
    myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
    -- Data.Map.union is left biased
    myKeys conf = myOverrides conf `Map.union` foldr Map.delete (keys def conf) (myRemoves conf)
      where
        myOverrides :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
        myOverrides XConfig {
          X.modMask    = modm
        , X.workspaces = workspaces
        } = Map.fromList $ [
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
              --   4
              -- 1 2 3
              | (scrId, key) <- zip screens [xK_a, xK_o, xK_e, xK_comma]
              , (mask, f)    <- [(noMask, W.view), (shiftMask, W.shift)]
          ] ++ [
            ((modm .|. mask, key), windows $ f wspId)
              | (wspId, key) <- zip workspaces [xK_1 ..]
              , (mask, f)    <- [
                  (noMask,      W.greedyView)
                , (shiftMask,   W.shift)
                , (controlMask, swapWithCurrent)
                ]
          ] ++ [
            ((modm, xK_0), withFocused $ windows . W.sink )
          ]
          where
            screens = [0..]
        myRemoves :: XConfig Layout -> [(ButtonMask, KeySym)]
        myRemoves XConfig { X.modMask = modm } = [
            -- remove default key for `sendMessage MirrorShrink`
            (modm, xK_l)
          ]
