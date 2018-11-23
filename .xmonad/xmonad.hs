import XMonad
import qualified XMonad.StackSet as W

import System.Taffybar.Support.PagerHints
import XMonad.Layout.Gaps
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWindows

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
  [ isFullscreen --> doFullFloat
  , isDialog --> doFloat
  , className =? "vlc" --> doFullFloat
  , className =? "Xfce4-notifyd" --> doIgnore
  , manageDocks]

myLogHook :: [X ()]
myLogHook =
 [ ewmhDesktopsLogHook >> setWMName "LG3D"  -- java workaround
 ]

main = xmonad $ docks $ ewmh def
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 2
  , manageHook = composeAll myManageHook
  , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
  , logHook = composeAll myLogHook
  , layoutHook = smartBorders $ avoidStruts $ myLayout
  , startupHook = do
      spawn "/bin/bash ~/.xmonadrc"
      spawn "feh --bg-fill /home/tjarvstrand/Pictures/Wallpapers/1.jpeg"
      spawn "taffybar"
      spawn "dropbox start -i"
      spawn "nm-applet"
      spawn "blueman-applet"
      spawn "xfce-power-manager"
      -- spawn "light-locker"
  }
  `additionalKeysP`
    [ ("M1-<Tab>",  windows W.focusDown)
    , ("M-w",  kill)
    , ("M-M1-<Space>", sendMessage ToggleStruts)
    , ("C-<Return>", spawn "dmenu_run_history -b -i")

    , ("<Print>", spawn "xfce4-screenshooter --fullscreen --save /home/tjarvstrand/Pictures/Screenshots")

    , ("XF86AudioMute",        spawn "/home/tjarvstrand/bin/toggle-mute")
    , ("XF86AudioLowerVolume", spawn "/home/tjarvstrand/bin/volume dec 10")
    , ("XF86AudioRaiseVolume", spawn "/home/tjarvstrand/bin/volume inc 10")
    , ("XF86MonBrightnessUp",  spawn "light -A 5")
    , ("XF86MonBrightnessDown", spawn "light -U 5")
    , ("C-XF86MonBrightnessUp", spawn "light -A 10")
    , ("C-XF86MonBrightnessDown", spawn "light -U 10")

    , ("C-M1-l", spawn "light-locker-command -l")
    ]
