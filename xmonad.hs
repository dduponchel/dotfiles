import Data.Monoid
import Data.Ratio ((%))
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map              as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Prompt         as P
import qualified XMonad.StackSet       as W

myTerminal = "urxvtc"

-- using the "windows key" instead of "left alt" (mod1Mask)
myModMask = mod4Mask

-- workspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- myWorkspaces = map show [1..10]
myWorkspaces = map concat (sequence [["u", "d"], (map show [1..10])])

-- Window rules
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = composeAll
    -- Allows focusing other monitors without killing the fullscreen
    [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
    {-
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Firefox"        --> doF (W.shift "web" )
    -}
    ]

-- The Java gui toolkit has a hardcoded list of so-called "non-reparenting" window managers.
-- xmonad is not on this list (nor are many of the newer window managers).
-- Attempts to run Java applications may result in `grey blobs' where windows should be,
-- as the Java gui code gets confused.
myStartupHook = setWMName "LG3D"

-- config for the Xmonad Prompt
myXPConfig = defaultXPConfig
    { fgColor  = "#55FF99"
    , bgColor  = "black"
    , bgHLight = "black"
    , fgHLight = "red"
    , position = Top
    }

-- Search engines to be selected
-- keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_y), method S.youtube )
       , ((0, xK_m), method S.maps )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
       , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
       , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
       , ((0, xK_d), method $ S.searchEngine "anidb" "http://anidb.net/perl-bin/animedb.pl?show=animelist&adb.search=")
       ]

-- Key bindings.
newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)
myKeys conf@(XConfig {XMonad.modMask = modMask}) =
    [ ((modMask .|. shiftMask, xK_z),  spawn "xscreensaver-command -lock")
    , ((0, xK_Print),                  spawn "scrot")
    , ((modMask, xK_Insert),           spawn "amixer -c 0 set Master 2dB+")
    , ((modMask, xK_Delete),           spawn "amixer -c 0 set Master 1dB-")
    , ((modMask, xK_Page_Up),          spawn "quodlibet --previous")
    , ((modMask, xK_Page_Down),        spawn "quodlibet --next")
    , ((modMask, xK_Home),             spawn "quodlibet --play-pause")
    , ((modMask, xK_quoteleft),        spawn "rotatexkbmap") -- with qwerty keyboard
    , ((modMask, xK_twosuperior),      spawn "rotatexkbmap") -- with azerty keyboard
    -- go to a named workspace
    , ((modMask, xK_g),                workspacePrompt myXPConfig (windows . W.shift))
    -- go to a window
    , ((modMask .|. shiftMask, xK_g),  goToSelected defaultGSConfig)
    -- search something, use the browser for the result
    , ((modMask, xK_s ),               SM.submap $ searchEngineMap $ S.promptSearchBrowser myXPConfig "firefox")
    -- like modMask + xK_p, but xmonad's shellprompt
    , ((modMask .|. shiftMask, xK_p), shellPrompt myXPConfig)
    -- toggle the space for the statusbar
    , ((modMask, xK_b ), sendMessage ToggleStruts)
    ]
    ++
    -- Switch workspaces (and move windows) vertically
    -- don't ask me how the code works, I really need to read a haskell book
    -- TODO : do I really need 20 workspaces ?
    [((keyMask .|. modMask, keySym), function (Lines 2) Finite direction)
     | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
     , (keyMask, function) <- [(0, planeMove), (shiftMask, planeShift)]
    ]
    ++
    -- same as /usr/share/xmonad-0.9.1/man/xmonad.hs, but add xK_0
    -- 10 workspaces !
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

-- dynamicLog for dzen. use xmobarPP for xmobar.
myLogHook = dynamicLogWithPP defaultPP
  { ppCurrent = dzenColor "yellow" "" . wrap "[" "]"
  , ppVisible = pad . wrap "<" ">"
  , ppHidden  = pad
  , ppUrgent  = dzenColor "red" "yellow"
  , ppTitle   = dzenColor "green"  "" . dzenEscape
  , ppWsSep   = ""
  }

-- Layouts
myLayout = tabbed ||| Grid ||| layoutHook defaultConfig
  where tabbed = named "Tabbed" $ simpleTabbed

-- do the job
main = do
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , terminal = myTerminal
        , modMask = myModMask
        , normalBorderColor="#000000"
        , focusedBorderColor="#009900"
        , borderWidth = 2
        , startupHook = myStartupHook
        , layoutHook = smartBorders (avoidStruts $ myLayout)
        , workspaces = myWorkspaces
        , keys = newKeys
        , logHook = myLogHook
        }
