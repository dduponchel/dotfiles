import XMonad
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

{-
 - I have more than one pc with xmonad, each with different uses and different programs.
 - This startup.conf file allows me to launch apps without any
 - "every-conf-pick-the-one-you-need" file in git.
 - syntax example for ~/.xmonad/startup.conf :
 - [InitProgram "workspace4" "urxvt", InitProgram "web" "firefox"]
 -}

-- data read from startup.conf
data InitProgram = InitProgram WorkspaceId String
  deriving (Read, Show)

-- read the startup.conf file
readInitProgram :: IO([InitProgram])
readInitProgram = do
  dir <- getXMonadDir
  strInitProgram <- readFile (dir ++ "/startup.conf")
  return $ read strInitProgram

-- this startup hook will use the startup conf and spawn the processes
myStartupHook = do
    setWMName "LG3D" -- java...
    listInitProgram <- liftIO readInitProgram
    mapM_ (\(InitProgram wid prg) -> spawnOn wid prg) listInitProgram

{-
 - That's it. Thank you Anthonin for this neat piece of code :)
 -}

-- using urxvtc
myTerminal = "urxvtc"

-- using the "windows key" instead of "left alt" (mod1Mask)
myModMask = mod4Mask

-- workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces = map show [1..10]
-- myWorkspaces = map concat (sequence [["u", "d"], (map show [1..10])])

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

{- spawn windows on launched workspace (instead of current workspace) -}
myDmenu :: X ()
myDmenu = do
  currentWorkspace <- fmap W.currentTag (gets windowset)
  spawnOn currentWorkspace "exe=`dmenu_path | dmenu ` && eval \"exec $exe\""
  -- Use the lastest sources ! I need this commit :
  -- http://git.suckless.org/dmenu/commit/?id=dec9a28863f388072be105e0950deb72ac719d48
  -- The current version (4.5) isn't good for my usecase : dmenu_path doesn't exist.


-- Key bindings.
newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)
myKeys conf@(XConfig {XMonad.modMask = modMask}) =
    [ ((modMask .|. shiftMask, xK_z),  spawn "resetxkbmap && xscreensaver-command -lock")
    {-- I must NOT fail to lock my session, even in azerty : my coworkers are quick ;-) --}
    , ((modMask .|. shiftMask, xK_w),  spawn "resetxkbmap && xscreensaver-command -lock")
    , ((0, xK_Print),                  spawn "scrot")
    , ((0, xF86XK_AudioRaiseVolume),   spawn "amixer set Master 2%+")
    , ((modMask, xK_Insert),           spawn "amixer set Master 2%+")
    , ((0, xF86XK_AudioLowerVolume),   spawn "amixer set Master 2%-")
    , ((modMask, xK_Delete),           spawn "amixer set Master 2%-")
    , ((modMask, xK_Page_Up),          spawn "quodlibet --previous")
    , ((modMask, xK_Page_Down),        spawn "quodlibet --next")
    , ((modMask, xK_Home),             spawn "quodlibet --play-pause")
    , ((modMask, xK_quoteleft),        spawn "rotatexkbmap") -- with qwerty keyboard
    , ((modMask, xK_twosuperior),      spawn "rotatexkbmap") -- with azerty keyboard
    -- go to a window
    , ((modMask .|. shiftMask, xK_g),  goToSelected defaultGSConfig)
    -- toggle the space for the statusbar
    , ((modMask, xK_b),                sendMessage ToggleStruts)
    -- set window fullscreen
    , ((modMask, xK_f),                sendMessage ToggleLayout)
    -- focus urgent window
    , ((modMask, xK_u),                focusUrgent)
    -- override default
    , ((modMask, xK_p),                myDmenu)
    ]
    ++
    -- same as /usr/share/xmonad-0.9.1/man/xmonad.hs, but add xK_0
    -- 10 workspaces !
    -- same as adding
    -- , ((modMask              , xK_0), windows $ W.greedyView (myWorkspaces!!9))
    -- , ((modMask .|. shiftMask, xK_0), windows $ W.shift      (myWorkspaces!!9))
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

-- dynamicLog for dzen. use xmobarPP or dzenPP for nice defaults
myLogHook namedPipe = dynamicLogWithPP defaultPP
  { ppOutput  = hPutStrLn namedPipe
  {-- small screens
  , ppCurrent = dzenColor "#09F" "" -- wrap "[" "]"
  , ppVisible = dzenColor "white" "" -- wrap "<" ">"
  -- , ppHiddenNoWindows  = dzenColor "#444" ""
  , ppUrgent  = dzenColor "red" "yellow"
  , ppTitle   = dzenEscape
  , ppWsSep   = " "
  --}
  {-- big screens --}
  , ppCurrent = dzenColor "#09F"  "" . pad -- wrap "[" "]"
  , ppVisible = dzenColor "white" "" . pad -- wrap "<" ">"
  , ppHidden  = pad
  , ppHiddenNoWindows  = dzenColor "#444" "" . pad
  , ppUrgent  = dzenColor "red" "yellow" . pad
  , ppTitle   = dzenEscape
  , ppWsSep   = ""
  } >> updatePointer (Relative 0.5 0.5) >> takeTopFocus

-- Layouts
myLayout = (toggleLayouts $ noBorders Full) $ -- toggle fullscreen
  (noBorders tabbed ||| Grid ||| layoutHook defaultConfig)
  where tabbed = named "Tabbed" $ simpleTabbed

-- do the job
main = do
    dir <- getXMonadDir
    {- namedPipe <- openFile (dir ++ "/output") AppendMode -}
    {- AppendMode doesn't works, see http://www.haskell.org/haskellwiki/GHC:FAQ#When_I_open_a_FIFO_.28named_pipe.29_and_try_to_read_from_it.2C_I_get_EOF_immediately. -}
    {- TODO : test on BSD ! -}
    namedPipe <- openFile (dir ++ "/output") ReadWriteMode
    hSetBuffering namedPipe LineBuffering
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        manageHook = manageSpawn <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
        , terminal = myTerminal
        , modMask = myModMask
        , normalBorderColor="#000000"
        , focusedBorderColor="#009900"
        , borderWidth = 2
        , startupHook = myStartupHook
        , layoutHook = smartBorders (avoidStruts $ myLayout)
        , workspaces = myWorkspaces
        , keys = newKeys
        , logHook = myLogHook namedPipe
        }
