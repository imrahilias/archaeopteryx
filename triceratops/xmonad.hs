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
--myFont = "xft:Bitstream Vera Sans Mono:pixelsize=12"       
--myFont               = "xft:Bitstream Vera Sans Mono:pixelsize=14" -- # "-*-fixed-medium-r-*-*-13-*-*-*-*-*-iso8859-*"
--dzenFont             = "xft:Bitstream Vera Sans Mono:pixelsize=17"
colorBlack           = "#020202" --Background
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#6a6464" --Gray (dzen2_fg)
colorGrayAlt         = "#303030" --Gray dark (dzen2_bg) alt: "#241e1e"
colorWhite           = "#a9a6af"
colorWhiteAlt        = "#9d9d9d"
colorMagenta         = "#8e82a2"
colorBlue            = "#39aaff" --darker: "#0778ec"
colorRed             = "#d74b73"
colorGreen           = "#99cc66"
colorOrange          = "#FF8E38" --"#ddaa00" --alt: "#bd5500"
myNormalBorderColor  = colorGray
myFocusedBorderColor = colorBlue

--------------------------------------------------------------------------------------------
-- WORKSPACES                                                                             --
--------------------------------------------------------------------------------------------
-- http://unicode-table.com/ » privat use zone
-- functioning cool symbols: ⚇♚⚉⚙⛃⛁⌨☣☢☮☯☭♾⇝⇜➤✆⚡⚛⚜⚔≣☠☄☎☏❍☺☻◌◉◎◍⊚⊙♬₪↯⇵⚶✉①➀➊②➁➋③➂➌④➃➍⑤➄➎☼☀◯◻◼☉★☆◈◇◆▢▣●○☷⌇‖❖⠶▦✯✭✭✫✶❂♪

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = [" ∅", "@", "$", "⛁", "≣", "☻", "♬", "◎", "↯", "➊", "➋", "➌", "➍", "➎"] 
--myWorkspaces = ["NULL", "WEB", "CODE", "FILE", "DOC", "CHAT", "AUDIO", "VIDEO", "LOAD", "Α", "Β", "Γ", "Δ", "Ε"] --never, NEVER name Workspaces identically!


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
		myWeb           = ["Iceweasel", "Firefox", "Opera", "Chromium"]
                myCode          = ["Emacs", "Emacs"]
                myFile          = ["Thunar", "Gentoo"]
                myDoc           = ["xpdf", "Xpdf", "Evince", "Evince-previewer", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-calc"]
		myChat          = ["psi", "Pidgin", "Skype", "Ts3client_linux_amd64"]
                myVideo         = ["Vlc", "MPlayer"]
                myLoad          = ["jd-Main"]
		myFloatCC       = ["Vlc", "MPlayer", "File-roller", "Nvidia-settings", "XFontSel", "XCalc", "XClock", "Main"]


--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-bg", colorGrayAlt, "-fg", colorGreen] }

-- StatusBars
myWorkspaceBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '0' -h '24' -w '1020' -ta 'l' -p -e '' -bg #ff0000"

-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) -- hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = "^fg(" ++ colorWhite ++ ") ⌇ "
	, ppWsSep           = " "
	, ppCurrent         = dzenColor colorOrange   colorGrayAlt . pad
	, ppUrgent          = dzenColor colorGreen    colorGrayAlt . pad
	, ppVisible         = dzenColor colorGray     colorGrayAlt . pad
	, ppHidden          = dzenColor colorWhite    colorGrayAlt . pad
	, ppHiddenNoWindows = dzenColor colorGray     colorGrayAlt . pad
        , ppLayout          = dzenColor colorBlue     colorGrayAlt . pad
	, ppTitle           = dzenColor colorWhiteAlt colorGrayAlt . pad
	}	where
		orderText (ws:l:t:_) = [ws,l,t] --display config


--------------------------------------------------------------------------------------------
-- KEYS                                                                                   --
--------------------------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ 
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
  , ((modm,               xK_p     ), spawn "`dmenu_run -nf '#A89C8C' -nb '#303030' -sf '#FF8E38' -sb '#5C5449' -fn '-*-fixed-*-*-*-*-20-*-*-*-*-*-iso8859-*'`") -- launch dmenu 4.5
  , ((modm .|. shiftMask, xK_p     ), spawn "`sudo dmenu_run -nf '#A89C8C' -nb '#303030' -sf '#FF8E38' -sb '#5C5449' -fn '-*-fixed-*-*-*-*-20-*-*-*-*-*-iso8859-*'`") -- launch dmenu 4.5
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
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart; conkykiller") --   
  , ((modm              , xK_KP_Subtract  ), spawn "unclutter -idle 1") -- mouse pointer invisible (defaults: /etc/default/unclutter)
  , ((modm              , xK_KP_Add  ), spawn "killall unclutter") -- fetch back mouse pointer:D
  , ((modm              , xK_e     ), spawn "emacsclient -c -a ''") -- Spawn Emacs (kinda runANDraise:) > emacsclient --help or ~/scripts/emc
  , ((modm              , xK_i     ), runOrRaise "chromium" (className =? "Chromium"))    -- Find or Spawn Chromium (i = internet)
  , ((modm              , xK_o     ), spawn "libreoffice")    -- Spawn libreoffice selection menu (o = office)
--, ((modm              , xK_w     ), runOrRaise "lowriter" (className =? "libreoffice-writer"))    -- Find or Spawn Writer
  , ((modm              , xK_f     ), spawn "thunar") -- Spawn Thunar (f = file)
  , ((modm .|. shiftMask, xK_f     ), spawn "sudo thunar") -- Spawn SUPERThunar (f = file)
-- multimedia
  , ((0,            0x1008ff13     ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
  , ((0,            0x1008ff11     ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
  , ((0,            0x1008ff12     ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
  , ((0,            0x1008ff81     ), safeSpawn "ncmpcpp" ["play"])   -- ncmpcpp > start playlist (moc running)
  , ((0,            0x1008ff16     ), safeSpawn "ncmpcpp" ["prev"])   -- ncmpcpp > previous track in playlist
  , ((0,            0x1008ff14     ), safeSpawn "ncmpcpp" ["toggle"])   -- ncmpcpp > toggle play/pause
  , ((0,            0x1008ff17     ), safeSpawn "ncmpcpp" ["next"])   -- ncmpcpp > next track in playlist
  , ((0,            0x1008ff2c     ), safeSpawn "ncmpcpp" ["stop"])   -- ncmpcpp > stop
  , ((0,            0x1008ff2c     ), do                                                 
                                        withTaggedGlobalP "ncmpcpp" (W.shiftWin (myWorkspaces !! 6))
                                        windows $ W.greedyView (myWorkspaces !! 6)
                                        runInTerm "" "ncmpcpp") -- XF86AudioPlay
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
