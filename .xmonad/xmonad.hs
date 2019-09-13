import XMonad
import qualified XMonad.StackSet as W

import Control.Monad
import Data.Function
import Data.List
import XMonad.Layout.Gaps
import qualified XMonad.Layout.Fullscreen as LF
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops as Ewmh
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWindows
import XMonad.Util.NamedWindows

import qualified XMonad.StackSet as W

myLayout = smartBorders tiled |||
           -- Mirror tiled |||
           noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
      nmaster = 1

    -- Default proportion of screen occupied by master pane
      ratio   = 2/3

    -- Percent of screen to increment by when resizing panes
      delta   = 3/100

myManageHook :: [ManageHook]
myManageHook =
  [ LF.fullscreenManageHook
  , isDialog --> doFloat
  , manageDocks]

myLogHook :: [X ()]
myLogHook =
 [ ewmhDesktopsLogHook >> setWMName "LG3D"  -- java workaround
 , eventLogHook
 , fadeInactiveLogHook 0.7
 ]

eventLogHook = do
  winset <- gets windowset
  let currWs = W.currentTag winset
  io $ appendFile "/home/tjarvstrand/.xmonad/workspace-log" (currWs ++ "\n")



main = xmonad $ docks $ ewmh def
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 1
  , manageHook =  composeAll myManageHook
  , handleEventHook = ewmhDesktopsEventHook <+> LF.fullscreenEventHook
  , logHook = composeAll myLogHook
  , layoutHook = smartBorders $ avoidStruts $ myLayout
  , startupHook = ewmhDesktopsStartup >> do
      spawn "/bin/bash ~/.xmonadrc"
      spawn "feh --bg-fill /home/tjarvstrand/Pictures/Wallpapers/1.jpeg"
  }
  `additionalKeysP`
    [ ("M1-<Tab>",  windows W.focusDown)
    , ("M-w",  kill)
    , ("M-M1-<Space>", sendMessage ToggleStruts)
    , ("C-<Return>", spawn "dmenu_run_history -b -i")
    , ("C-M1-l", spawn "light-locker-command -l")

    , ("<Print>", spawn "xfce4-screenshooter --fullscreen --save /home/tjarvstrand/Pictures/Screenshots")

    , ("XF86Suspend", spawn "systemctl suspend")
    , ("XF86WLAN", spawn "toggle-wifi")

    -- , ("<XF86AudioMute>",        spawn "/home/tjarvstrand/bin/toggle-mute")
    -- , ("<XF86AudioLowerVolume>", spawn "/home/tjarvstrand/bin/volume dec 5")
    -- , ("<XF86AudioRaiseVolume>", spawn "/home/tjarvstrand/bin/volume inc 5")
    , ("<XF86MonBrightnessUp>",  spawn "/home/tjarvstrand/bin/backlight inc 5")
    , ("<XF86MonBrightnessDown>", spawn "/home/tjarvstrand/bin/backlight dec 5")
    , ("C-<XF86MonBrightnessUp>", spawn "/home/tjarvstrand/bin/backlight inc 10")
    , ("C-<XF86MonBrightnessDown>", spawn "/home/tjarvstrand/bin/backlight dec 10")
    ]
