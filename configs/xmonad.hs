-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile && xmonad --restart

--------------------------------------------------------------------------------------------
-- IMPORT                                                                                 --
--------------------------------------------------------------------------------------------

import XMonad
import XMonad.Core
import System.IO (Handle, hPutStrLn)
import Data.Monoid
import Data.List
import XMonad.StackSet (RationalRect (..), currentTag)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import Graphics.X11.ExtraTypes.XF86

-- hooks
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder) -- smartBorders eliminates border in fullscreen mode
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize

-- utils
import XMonad.Util.Replace
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig(additionalKeys)

-- actions
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Actions.TagWindows

--------------------------------------------------------------------------------------------
-- MAIN                                                                                   --
--------------------------------------------------------------------------------------------

main :: IO()
main = do
        workspaceBar            <- spawnPipe myWorkspaceBar
	replace
	xmonad $ myUrgencyHook $ defaultConfig
  	      { terminal = "urxvt"
	      , modMask = mod4Mask -- Rebind Mod to the Windows key
              , keys                    = myKeys
	      , borderWidth 		= 1
	      , normalBorderColor	= myNormalBorderColor
	      , focusedBorderColor	= myFocusedBorderColor
	      , manageHook		= manageDocks <+> myManageHook
	      , layoutHook 		= smartBorders $ avoidStruts  $ layoutHook defaultConfig --avoidStruts verhindert überschreiben von dzen
	      , logHook          	= (myLogHook workspaceBar)
              , workspaces              = myWorkspaces
              }


--------------------------------------------------------------------------------------------
-- APPEARANCE CONFIG                                                                      --
--------------------------------------------------------------------------------------------

-- Colors and fonts
myFont               = "-*-fixed-medium-r-*-*-13-*-*-*-*-*-iso8859-*"
dzenFont             = myFont
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#3955c4"
colorRed             = "#d74b73"
colorGreen           = "#99cc66"
colorOrange          = "#bd5500"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorBlue


--------------------------------------------------------------------------------------------
-- WORKSPACES                                                                             --
--------------------------------------------------------------------------------------------

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["NULL", "WEB", "CODE", "FILE", "DOC", "CHAT", "AUDIO", "VIDEO", "LOAD", "Α", "Β", "Γ", "Δ", "Ε"] --never, NEVER name Workspaces identically!


--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- Scratchpad (W+º)
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (0) (1/50) (1) (3/4))
scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad"

-- Manage hook
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
	[ [className    =? c     --> doShiftAndGo (myWorkspaces !! 1)     | c <- myWeb] --move myWeb windows to workspace 1 by classname
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 2)     | c <- myCode]
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 3)     | c <- myFile]
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 4)     | c <- myDoc]
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 5)     | c <- myChat]
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 7)     | c <- myVideo]
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 8)     | c <- myLoad]
	, [className    =? c     --> doCenterFloat                        | c <- myFloatCC]
--	, [name         =? n     --> doCenterFloat                        | n <- myFloatCN]
--	, [name         =? n     --> doSideFloat NW                       | n <- myFloatSN]
--	, [className    =? c     --> doF W.focusDown                      | c <- myFocusDC]
--      , [resource     =? r     --> doIgnore                             | r <- myIgnores]
	, [isFullscreen          --> doFullFloat]
	]) <+> manageScratchPad
	where
		doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
		role            = stringProperty "WM_WINDOW_ROLE"
		name            = stringProperty "WM_NAME"
		myWeb           = ["Iceweasel", "Firefox", "Opera"]
                myCode          = ["Emacs", "Emacs"]
                myFile          = ["Thunar", "Gentoo"]
                myDoc           = ["xpdf", "Xpdf", "evince", "Evince", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-calc"]
		myChat          = ["psi", "Pidgin", "Skype", "Ts3client_linux_amd64"]
                myVideo         = ["Vlc", "Mplayer"]
                myLoad          = ["jd-Main"]
		myFloatCC       = ["Vlc", "MPlayer", "File-roller", "Nvidia-settings", "XFontSel", "XCalc", "XClock", "Main"]


--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }

-- StatusBars
myWorkspaceBar :: String
myWorkspaceBar    = "dzen2 -x '5' -y '0' -h '16' -w '1420' -ta 'l' -p -e '' -bg #000000"

-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) -- hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = "^fg(" ++ colorGray ++ ")|"
	, ppWsSep           = ""
--	, ppCurrent         = dzenColor colorBlue     colorBlack . pad
	, ppCurrent         = dzenColor colorOrange   colorBlack . pad
	, ppUrgent          = dzenColor colorGreen    colorBlack . pad 
	, ppVisible         = dzenColor colorGray     colorBlack . pad
	, ppHidden          = dzenColor colorWhiteAlt colorBlack . pad
	, ppHiddenNoWindows = dzenColor colorGray     colorBlack . pad
        , ppLayout          = dzenColor colorBlue     colorBlack . pad
	, ppTitle           = dzenColor colorWhiteAlt colorBlack . pad
	}
	where
		orderText (ws:l:t:_) = [ws,l,t] --display config


--------------------------------------------------------------------------------------------
-- KEYS                                                                                   --
--------------------------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ 
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
  , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- launch dmenu
  , ((modm .|. shiftMask, xK_c     ), kill) -- close focused window
  , ((modm,               xK_space ), sendMessage NextLayout)     -- Rotate through the available layout algorithms
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)    --  Reset the layouts on the current workspace to default
  , ((modm,               xK_n     ), refresh)    -- Resize viewed windows to the correct size
  , ((modm,               xK_Tab   ), windows W.focusDown)    -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)    -- Move focus to the next window
  , ((modm,               xK_k     ), windows W.focusUp)    -- Move focus to the previous window
  , ((modm,               xK_m     ), windows W.focusMaster)    -- Move focus to the master window
  , ((modm,               xK_Return), windows W.swapMaster)    -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)    -- Swap the focused window with the previous window
  , ((modm,               xK_h     ), sendMessage Shrink)    -- Shrink the master area
  , ((modm,               xK_l     ), sendMessage Expand)    -- Expand the master area
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)    -- Push window back into tiling
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))    -- Increment the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))    -- Deincrement the number of windows in the master area
  , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))   -- Quit xmonad
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") --   
  , ((modm              , xK_KP_Subtract  ), spawn "unclutter -idle 1") -- mouse pointer invisible (defaults: /etc/default/unclutter)
  , ((modm              , xK_KP_Add  ), spawn "killall unclutter") -- fetch back mouse pointer:D
  , ((modm              , xK_e     ), spawn "emacsclient -c -a ''") -- Spawn Emacs (kinda runANDraise:) > emacsclient --help or ~/scripts/emc
--, ((modm              , xK_e     ), spawn "emc") -- Spawn Emacs (kinda runANDraise:)
--, ((modm              , xK_e     ), runOrRaise "emc" (className =? "Emacs"))    -- Find or Spawn Emacs
  , ((modm              , xK_i     ), runOrRaise "opera" (className =? "Opera"))    -- Find or Spawn Opera (i = internet)
  , ((modm              , xK_o     ), spawn "libreoffice")    -- Spawn libreoffice selection menu (o = office)
--, ((modm              , xK_w     ), runOrRaise "lowriter" (className =? "libreoffice-writer"))    -- Find or Spawn Writer
  , ((modm              , xK_f     ), spawn "thunar") -- Spawn Thunar (f = file)
  , ((modm .|. shiftMask, xK_f     ), spawn "sudo thunar") -- Spawn SUPERThunar (f = file)
  , ((modm              , xK_g     ), spawn "gentoo --root-ok") -- Spawn Gentoo as root (g = gentoo, file manager #2)
-- multimedia
--, ((modm              , xK_z     ), raiseMaybe (runInTerm "-class mocp" "mocp") (title =? "mocp"))
--, ((modm              , xK_z     ), raiseMaybe (runInTerm "-title mutt" "mutt") (title =? "mutt"))
  , ((0,            0x1008ff13     ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
  , ((0,            0x1008ff11     ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
  , ((0,            0x1008ff12     ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
  , ((0,            0x1008ff81     ), safeSpawn "mocp" ["-p"])   -- moc > start playlist (moc running)
  , ((0,            0x1008ff16     ), safeSpawn "mocp" ["-r"])   -- moc > previous track in playlist
  , ((0,            0x1008ff14     ), safeSpawn "mocp" ["-G"])   -- moc > toggle play/pause
  , ((0,            0x1008ff17     ), safeSpawn "mocp" ["-f"])   -- moc > next track in playlist
--, ((0,            0x1008ff2c     ), safeSpawn "mocp" ["-s"])   -- moc > stop
  , ((0,            0x1008ff2c     ), do                                                 
                                        withTaggedGlobalP "mocp" (W.shiftWin (myWorkspaces !! 6))
                                        windows $ W.greedyView (myWorkspaces !! 6)
                                        runInTerm "" "mocp") -- XF86AudioPlay
  ]
  ++

-- Screenshot commands
  [ ((0,            xK_Print       ), spawn "screen jpg")
  , ((modm,         xK_Print       ), spawn "screen png")
  ]
  ++

-- mod-[1..9], Switch to workspace N, mod-shift-[1..9], Move client to workspace N
  [
    ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) ([0xfe52] ++ [xK_1 .. xK_9] ++ [xK_0, 0xdf, 0xfe51, xK_BackSpace]) --0xdf ff. depends on german key layout
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]