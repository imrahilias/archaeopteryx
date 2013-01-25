--------------------------------------------------------------------------------------------
-- AUTHOR: M										  --
--------------------------------------------------------------------------------------------
-- AUTHOR: nnoell <nnoell3@gmail.com>                                                     --
--------------------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                                    --
-- validate syntax: xmonad --recompile                                                    --
--------------------------------------------------------------------------------------------

-- Misc
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}

-- Imported libraries
import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.StackSet (RationalRect (..), currentTag)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Replace
import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import Data.Monoid
import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle, hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex

-- Main
main :: IO()
main = do
        workspaceBar            <- spawnPipe myWorkspaceBar
        --bottomStatusBar         <- spawnPipe myBottomStatusBar
        --topStatusBar            <- spawnPipe myTopStatusBar --------------------------------------------------------
	replace
	xmonad $ myUrgencyHook $ defaultConfig
  	      { terminal = "urxvt"
	      , modMask			= mod4Mask     -- Rebind Mod to the Windows key
	      , borderWidth 		= 1
	      , normalBorderColor	= myNormalBorderColor
	      , focusedBorderColor	= myFocusedBorderColor
	      , manageHook		= manageDocks <+> manageHook defaultConfig
	      , layoutHook 		= avoidStruts  $  layoutHook defaultConfig
	      , logHook          	= (myLogHook workspaceBar)
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
myArrow              = "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorBlue

-- Tab theme
myTabTheme :: Theme
myTabTheme = defaultTheme
	{ fontName            = myFont
	, inactiveBorderColor = colorBlackAlt
	, inactiveColor       = colorBlack
	, inactiveTextColor   = colorGray
	, activeBorderColor   = colorGray
	, activeColor         = colorBlackAlt
	, activeTextColor     = colorWhiteAlt
	, urgentBorderColor   = colorGray
	, urgentTextColor     = colorGreen
	, decoHeight          = 14
	}

-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
	{ font                = myFont
	, bgColor             = colorBlack
	, fgColor             = colorWhite
	, bgHLight            = colorBlue
	, fgHLight            = colorWhite
	, borderColor         = colorGrayAlt
	, promptBorderWidth   = 1
	, height              = 16
	, position            = Top
	, historySize         = 100
	, historyFilter       = deleteConsecutive
	, autoComplete        = Nothing
	}

-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) -- lowest inactive bg
	(0x1C,0x1C,0x1C) -- highest inactive bg
	(0x39,0x55,0xC4) -- active bg
	(0xBB,0xBB,0xBB) -- inactive fg
	(0x00,0x00,0x00) -- active fg

-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight  = 50
	, gs_cellwidth   = 200
	, gs_cellpadding = 10
	, gs_font        = myFont
	}

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["TERM", "WEBS", "CODE", "GRFX", "CHAT", "GAME", "VIDS", "OTHR"]


--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------

-- Layouts
myTile = named "T"  $ smartBorders $ ResizableTall 1 0.03 0.5 []
myMirr = named "MT" $ smartBorders $ Mirror myTile
myMosA = named "M"  $ smartBorders $ MosaicAlt M.empty
myObig = named "O"  $ smartBorders $ OneBig 0.75 0.65
myTabs = named "TS" $ smartBorders $ tabbed shrinkText myTabTheme
myFull = named "TS" $ smartBorders $ tabbedAlways shrinkText myTabTheme
myTabM = named "TM" $ smartBorders $ mastered 0.01 0.4 $ tabbed shrinkText myTabTheme
myGimp = named "G"  $ withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") myMosA
myChat = named "C"  $ withIM (0.20) (Title "Buddy List") $ Mirror $ ResizableTall 1 0.03 0.5 []

-- Transformers (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFull (\_ -> x)

-- Layout hook
myLayoutHook = gaps [(U,16), (D,16), (L,0), (R,0)]
	$ avoidStruts
	$ minimize
	$ mkToggle (single TABBED)
	$ mkToggle (single MIRROR)
	$ mkToggle (single REFLECTX)
	$ mkToggle (single REFLECTY)
	$ onWorkspace (myWorkspaces !! 1) webLayouts  --Workspace 1 layouts
	$ onWorkspace (myWorkspaces !! 2) codeLayouts --Workspace 2 layouts
	$ onWorkspace (myWorkspaces !! 3) gimpLayouts --Workspace 3 layouts
	$ onWorkspace (myWorkspaces !! 4) chatLayouts --Workspace 4 layouts
	$ allLayouts
	where
		allLayouts  = myTile ||| myObig ||| myMirr ||| myMosA ||| myTabM
		webLayouts  = myTabs ||| myTabM
		codeLayouts = myTabM ||| myTile
		gimpLayouts = myGimp
		chatLayouts = myChat


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
	[ [resource     =? r     --> doIgnore                             | r <- myIgnores] --ignore desktop
	, [className    =? c     --> doShift (myWorkspaces !! 1)          | c <- myWebS   ] --move myWebS windows to workspace 1 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 4)          | c <- myChatS  ] --move myChatS windows to workspace 4 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 3)          | c <- myGfxS   ] --move myGfxS windows to workspace 4 by classname
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 5)     | c <- myGameS  ] --move myGameS windows to workspace 5 by classname and shift
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 7)     | c <- myOtherS ] --move myOtherS windows to workspace 5 by classname and shift
	, [className    =? c     --> doCenterFloat                        | c <- myFloatCC] --float center geometry by classname
	, [name         =? n     --> doCenterFloat                        | n <- myFloatCN] --float center geometry by name
	, [name         =? n     --> doSideFloat NW                       | n <- myFloatSN] --float side NW geometry by name
	, [className    =? c     --> doF W.focusDown                      | c <- myFocusDC] --dont focus on launching by classname
	, [isFullscreen          --> doFullFloat]
	]) <+> manageScratchPad
	where
		doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
		role            = stringProperty "WM_WINDOW_ROLE"
		name            = stringProperty "WM_NAME"
		myIgnores       = ["desktop","desktop_window"]
		myWebS          = ["Chromium","Firefox", "Opera"]
		myGfxS          = ["Gimp", "gimp", "GIMP"]
		myChatS         = ["Psi","psi", "Pidgin", "Xchat"]
		myGameS         = ["zsnes"]
		myOtherS        = ["Amule", "Transmission-gtk"]
		myFloatCC       = ["MPlayer", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1", "Gksu", "PSX", "Galculator", "Nvidia-settings", "XFontSel", "XCalc", "XClock", "Desmume", "Ossxmix", "Xvidcap", "Main", "Wicd-client.py", "com-mathworks-util-PostVMInit"]
		myFloatCN       = ["ePSXe - Enhanced PSX emulator", "Seleccione Archivo", "Config Video", "Testing plugin", "Config Sound", "Config Cdrom", "Config Bios", "Config Netplay", "Config Memcards", "About ePSXe", "Config Controller", "Config Gamepads", "Select one or more files to open", "Add media", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", ""]
		myFloatSN       = ["Event Tester"]
		myFocusDC       = ["Event Tester", "Notify-osd"]


--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }

-- StatusBars
myWorkspaceBar, myBottomStatusBar :: String
myWorkspaceBar    = "dzen2 -x '8' -y '0' -h '16' -w '960' -ta 'l' -p -e ''"
myBottomStatusBar = "/usr/bin/dzconky.sh"
--myTopStatusBar    = "/usr/bin/topstatusbar.sh"

-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) -- hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = "^fg(" ++ colorGray ++ ")|"
	, ppWsSep           = ""
	, ppCurrent         = dzenColor colorBlue     colorBlack . pad
	, ppUrgent          = dzenColor colorGreen    colorBlack . pad 
	, ppVisible         = dzenColor colorGray     colorBlack . pad
	, ppHidden          = dzenColor colorWhiteAlt colorBlack . pad
	, ppHiddenNoWindows = dzenColor colorGray     colorBlack . pad
	, ppLayout          = dzenColor colorBlue     colorBlack . pad
	, ppTitle           = dzenColor colorWhiteAlt colorBlack . pad
	}
	where
		--display config
		orderText (ws:l:t:_) = [ws,l,t]
		titleText [] = "Desktop " ++ myArrow
		titleText x = (shorten 82 x) ++ " " ++ myArrow
		layoutText "Minimize T"  = "ReTall"
		layoutText "Minimize O"  = "OneBig"
		layoutText "Minimize TS" = "Tabbed"
		layoutText "Minimize TM" = "Master"
		layoutText "Minimize M"  = "Mosaic"
		layoutText "Minimize MT" = "Mirror"
		layoutText "Minimize G"  = "Mosaic"
		layoutText "Minimize C"  = "Mirror"
		layoutText "Minimize ReflectX T"  = "^fg(" ++ colorGreen ++ ")ReTall X^fg()"
		layoutText "Minimize ReflectX O"  = "^fg(" ++ colorGreen ++ ")OneBig X^fg()"
		layoutText "Minimize ReflectX TS" = "^fg(" ++ colorGreen ++ ")Tabbed X^fg()"
		layoutText "Minimize ReflectX TM" = "^fg(" ++ colorGreen ++ ")Master X^fg()"
		layoutText "Minimize ReflectX M"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		layoutText "Minimize ReflectX MT" = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		layoutText "Minimize ReflectX G"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		layoutText "Minimize ReflectX C"  = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		layoutText "Minimize ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall Y^fg()"
		layoutText "Minimize ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig Y^fg()"
		layoutText "Minimize ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed Y^fg()"
		layoutText "Minimize ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master Y^fg()"
		layoutText "Minimize ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		layoutText "Minimize ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		layoutText "Minimize ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		layoutText "Minimize ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		layoutText "Minimize ReflectX ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall XY^fg()"
		layoutText "Minimize ReflectX ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig XY^fg()"
		layoutText "Minimize ReflectX ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed XY^fg()"
		layoutText "Minimize ReflectX ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master XY^fg()"
		layoutText "Minimize ReflectX ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		layoutText "Minimize ReflectX ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		layoutText "Minimize ReflectX ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		layoutText "Minimize ReflectX ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		layoutText x = "^fg(" ++ colorGreen ++ ")" ++ x ++ "^fg()"
