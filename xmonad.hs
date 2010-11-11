--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.IO

import XMonad.Hooks.FadeInactive
import XMonad.Util.EZConfig(additionalKeys)
import Data.Ratio ((%))
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid

import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "urxvt"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    -- Allows focusing other monitors without killing the fullscreen
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- http://bbs.archlinux.org/viewtopic.php?pid=744649
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
myKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((0, xK_Print),                  spawn "scrot")
    , ((mod4Mask, xK_Up),              spawn "amixer -c 0 set Master 2dB+")
    , ((mod4Mask, xK_Down),            spawn "amixer -c 0 set Master 1dB-")
    , ((mod4Mask, xK_Page_Up),         spawn "quodlibet --previous")
    , ((mod4Mask, xK_Page_Down),       spawn "quodlibet --next")
    , ((mod4Mask, xK_Home),            spawn "quodlibet --play-pause")
    , ((mod4Mask, xK_quoteleft),       spawn "rotatexkbmap") -- with qwerty keyboard
    , ((mod4Mask, xK_twosuperior),     spawn "rotatexkbmap") -- with azerty keyboard
    ]

myLogHook = dynamicLogWithPP dzenPP

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = Grid ||| tiled ||| Mirror tiled ||| simpleTabbed ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmonad $ defaultConfig {
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , terminal = myTerminal
        , modMask = myModMask
        , normalBorderColor="#000044"
        , focusedBorderColor="#990000"
        , borderWidth = 1
        , startupHook = myStartupHook
        , layoutHook = smartBorders (avoidStruts $ myLayout)
        , workspaces = myWorkspaces
        , keys = newKeys
        , logHook = myLogHook
        }
