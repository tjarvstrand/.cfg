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
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWindows
import XMonad.Actions.UpdatePointer
import XMonad.Actions.PhysicalScreens
import XMonad.Util.NamedWindows
import XMonad.Util.EZConfig

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
 , updatePointer (0.0, 0.0) (0, 0)
 , eventLogHook
 ]

printWorkspaceLog screen =
  let S screenId = W.screen screen
      tag = W.tag $ W.workspace screen
      file = "/home/tjarvstrand/.xmonad/workspace-log-" ++ (show screenId)
  in
    io $ appendFile file (tag ++ "\n")

eventLogHook = do
  winset <- gets windowset
  forM_ (W.current winset: W.visible winset) printWorkspaceLog


myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

keybindings =
    [ ("M1-<Tab>",  windows W.focusDown)
    , ("M-w",  kill)
    , ("M-M1-<Space>", sendMessage ToggleStruts)
    , ("C-<Return>", spawn "dmenu_run_history -b -i")
    , ("C-M1-l", spawn "light-locker-command -l")

    , ("<Print>", spawn "xfce4-screenshooter --fullscreen --save /home/tjarvstrand/Pictures/Screenshots")

    , ("XF86Suspend", spawn "systemctl suspend")
    , ("XF86WLAN", spawn "toggle-wifi")

    -- , ("<XF86AudioMute>",        spawn "toggle-mute")
    -- , ("<XF86AudioLowerVolume>", spawn "volume dec 5")
    -- , ("<XF86AudioRaiseVolume>", spawn "volume inc 5")
    , ("<XF86MonBrightnessUp>",  spawn "backlight inc 5")
    , ("<XF86MonBrightnessDown>", spawn "backlight dec 5")
    , ("C-<XF86MonBrightnessUp>", spawn "backlight inc 10")
    , ("C-<XF86MonBrightnessDown>", spawn "backlight dec 10")
    ] ++
    [ (otherModMasks ++ "M-" ++ key, action key)
    | key  <- myWorkspaces
    , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                 , ("S-", windows . W.shift)]
    ]

main = xmonad $ docks $ ewmh def
  { modMask = mod4Mask
  , workspaces = myWorkspaces
  , terminal = "urxvt"
  , borderWidth = 1
  , manageHook =  composeAll myManageHook
  , handleEventHook = ewmhDesktopsEventHook <+> LF.fullscreenEventHook
  , logHook = composeAll myLogHook
  , layoutHook = smartBorders $ avoidStruts $ myLayout
  , startupHook = ewmhDesktopsStartup >> do
      spawn "/bin/bash ~/.xmonadrc"
  }
  `additionalKeysP` keybindings
