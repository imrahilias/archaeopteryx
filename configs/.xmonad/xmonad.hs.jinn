-- import stuff
import XMonad
import System.IO
import System.Exit
import Data.Monoid
import Data.Ratio ((%))
import qualified XMonad.StackSet as W  
import qualified Data.Map        as M
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Cursor

-- hooks
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog  
import XMonad.Hooks.ManageDocks

-- util
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Util.Cursor

-- layouts
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import Control.OldException(catchDyn,try)
import XMonad.Layout.ComboP
import XMonad.Layout.Column
import XMonad.Layout.Named
import XMonad.Layout.TwoPane

myModMask = mod4Mask
newManageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig


-- Color of current window title in xmobar.
xmobarTitleColor = "#D88B83"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#A9E8A9"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }
-- borders
myBorderWidth = 1
myNormalBorderColor = "#222222"
myFocusedBorderColor = "#ffffff"
  


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
                   { borderWidth        = myBorderWidth
                   , terminal           = "urxvt"
                   , normalBorderColor  = myNormalBorderColor
                   , focusedBorderColor = myFocusedBorderColor
                   , modMask = myModMask -- Rebind Mod to Windows key
                               
                   --, layoutHook = avoidStruts  $  layoutHook defaultConfig 
                   , layoutHook = avoidStruts $ myLayoutHook
                   , logHook = dynamicLogWithPP xmobarPP
                               { ppOutput = hPutStrLn xmproc
                               , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
                               , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                               , ppSep = "    "
                               }
                   , manageHook = newManageHook
                   , workspaces = myWorkspaces
                   , startupHook = setDefaultCursor xC_pirate
                   , keys = myKeys
                    } `additionalKeys`
                   [ ((mod4Mask .|. shiftMask,  xK_Return ), spawn "xterm")
                   , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
                   , ((0, xK_Print), spawn "scrot")
                   ]

-- layout 
myLayoutHook = onWorkspace "0-tor" tabonly $
               onWorkspace "1-chat" grid $
               tile ||| mtile ||| tab ||| full
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = renamed [Replace "[]="] $ smartBorders rt
    mtile = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
    tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full = renamed [Replace "[]"] $ noBorders Full 
    grid = renamed [Replace "IM"] $ withIM (1%8) (Title "Psi") Grid
    tabonly = tab

-- manage hook
myManageHook :: ManageHook
myManageHook = composeAll
               [ className =? "psi" --> doF (W.shift "1-chat")
               , className =? "Emacs" --> doF (W.shift "3-code")
               , className =? "Evince" --> doF (W.shift "4-pdf")
               , className =? "Krusader" --> doF (W.shift "5-fman")
               , className =? "Iceweasel" --> doF (W.shift "7-web")
               , className =? "MPlayer" --> doF (W.shift "8-vid")                 
               ]


-- workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["0-tor", "1-chat", "2-mail", "3-code", "4-pdf", "5-fman", "6-web", "7-web", "8-vid", "9-media", "10-misc", "11-misc", "12-misc", "13-misc"]
               

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

-- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  
    -- launch dmenu
  , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    
    -- close focused window
  , ((modm .|. shiftMask, xK_c     ), kill)
    
    -- Rotate through the available layout algorithms
  , ((modm,               xK_space ), sendMessage NextLayout)
    
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    
    -- Resize viewed windows to the correct size
  , ((modm,               xK_n     ), refresh)
    
    -- Move focus to the next window
  , ((modm,               xK_Tab   ), windows W.focusDown)
    
    -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)
    
    -- Move focus to the previous window
  , ((modm,               xK_k     ), windows W.focusUp  )
    
    -- Move focus to the master window
  , ((modm,               xK_m     ), windows W.focusMaster  )
    
    -- Swap the focused window and the master window
  , ((modm,               xK_Return), windows W.swapMaster)
    
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    
    -- Shrink the master area
  , ((modm,               xK_h     ), sendMessage Shrink)
    
    -- Expand the master area
  , ((modm,               xK_l     ), sendMessage Expand)
    
    -- Push window back into tiling
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    
    -- Increment the number of windows in the master area
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    
    -- Deincrement the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    
    -- Find or Spawn Iceweasel
  , ((modm              , xK_i), runOrRaise "iceweasel" (className =? "Iceweasel"))
    
    -- Find or Spawn Emacs
  , ((modm              , xK_u), runOrRaise "ec" (className =? "Emacs"))

    -- Find or Spawn Krusader
  , ((modm              , xK_o), runOrRaise "krusader" (className =? "Krusader"))

    -- multimedia  
  , ((0,            0x1008ff13), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
  , ((0,            0x1008ff11), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
  , ((0,            0x1008ff12), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
  , ((0,            0x1008ff14), safeSpawn "mocp" ["-G"])
  , ((0,            0x1008ff15), safeSpawn "mocp" ["-s"])
  , ((0,            0x1008ff16), safeSpawn "mocp" ["-r"])
  , ((0,            0x1008ff17), safeSpawn "mocp" ["-f"])
    
    
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
  , ((modm              , xK_b     ), sendMessage ToggleStruts)
    
    -- Quit xmonad
  , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    
    -- Restart xmonad
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
  ]
  ++
  
  --
  -- mod-[1..9], Switch to workspace N
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace]) -- y u no work?
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  
  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  