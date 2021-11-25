-- XMonad configuration
--
-- [[https://xmonad.org/][XMonad]] is a dynamically tiling window manager
-- written and configured in [[https://www.haskell.org/][Haskell]]. It allows
-- you to get things done by automating the aligning and positioning of the
-- windows you open.

-- Import libraries
--
-- Most of the libraries used here come from the package =xmonad-contrib=.

-- Base
import XMonad
import System.Exit (exitSuccess)
import System.Directory
import qualified XMonad.StackSet as W
import GHC.IO.Handle.Types
import qualified Codec.Binary.UTF8.String as UTF8

-- Config
import XMonad.Config.Desktop -- desktopConfig

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks -- Automatically manage dock type programs (panel, mainly)
import XMonad.Hooks.DynamicLog -- For panel
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.FadeInactive -- Add transparency to inactive windows
import XMonad.Hooks.UrgencyHook

-- Util
import XMonad.Util.SpawnOnce
import XMonad.Util.Run -- For spawn
import XMonad.Util.EZConfig -- For custom keys
import XMonad.Util.NamedActions
import XMonad.Util.Cursor

-- Actions
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.GroupNavigation
import XMonad.Actions.SpawnOn

-- Layout
import XMonad.Layout.Spacing -- Add spacing (gaps) between windows
import XMonad.Layout.ToggleLayouts -- For toggling layouts
import XMonad.Layout.Grid -- For Grid layout
import XMonad.Layout.Renamed -- For renaming workspaces

-- Data
import qualified Data.Map as M
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- DBUS
import qualified DBus as D
import qualified DBus.Client as D

--import Graphics.X11.ExtraTypes.XF86 -- For Brightness keys
--import System.IO (hClose)
--import System.Posix.Process(executeFile)

-- Change prefix key
--
-- =modMask= lets you specify which modkey you want to use. The default is
-- =mod1Mask= ("left Alt"). You may also consider using =mod3Mask= ("right
-- Alt"), which does not conflict with emacs keybindings. I prefer the "windows
-- key", which is usually =mod4Mask=.
vctModMask :: KeyMask
vctModMask = mod4Mask

-- Custom Variables
--
-- Here I set some values for things like font, terminal and editor. With this I
-- only have to change the value here to make modifications globally.

-- Application launcher
vctLauncher :: String
vctLauncher = "rofi -sidebar-mode -modi 'run,drun,ssh' -show 'run'"

-- Terminal
--
-- I like to be able to use the same terminal environment no matter which
-- workspace I'm using. Therefore I always keep my terminal connected to a tmux
-- session named =vct=.
vctTerminal :: String
--vctTerminal = "xterm -e 'tmux attach-session -t vct'"
vctTerminal = "xterm"

-- Screensaver
vctScreenSaver :: String
vctScreenSaver = "xscreensaver-command -lock"

-- Editor
vctEditor :: String
vctEditor = "emacsclient --create-frame --alternate-editor=''"

-- Web browser
vctWebBrowser :: String
vctWebBrowser = "firefox"

vctFileManager :: String
vctFileManager = "xterm -e ranger"

-- Width of Windows border
--
-- Change this to a value > 0 to have border:
vctBorderWidth :: Dimension
vctBorderWidth = 4

-- Colors
-- Colorscheme
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Set Border colors
-- The usual behaviour of XMonad for highlight the focused (active) window is to
-- draw a (usually red) border around it. You can change the color and width of
-- the borders as follows:
vctFocusedBorderColor :: String
vctFocusedBorderColor = "#5294E2"
vctNormalBorderColor :: String
vctNormalBorderColor = "#282c34"

-- Workspaces
vctWorkspaces :: [String]
vctWorkspaces = ["1:WWW", "2:DEV", "3:READ", "4:AUX", "5:EXTRA"] ++ map show [6..9]

-- Keybindings
vctKeys :: String -> [([Char], X ())]
vctKeys home =
  -- XMonad
  [ ("M-q", spawn "xmonad --restart") -- Recompiles XMonad
  , ("M-S-q", io exitSuccess)  -- Exits XMonad
  -- Programs
  , ("M-p", spawn vctLauncher)
  , ("M-a", spawn vctEditor)
  , ("M-z", sendMessage (Toggle "Full"))
  , ("M-S-z", spawn vctScreenSaver)
  , ("M-f", spawn vctFileManager)
  -- Increase/decrease spacing (gaps)
  , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
  , ("M-i", incWindowSpacing 4)           -- Increase window spacing
  , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
  , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing
  -- Kill windows
  , ("M-S-c", kill1)     -- Kill the currently focused client
  , ("M-S-a", killAll)   -- Kill all windows on current workspace
  -- Windows navigation
  , ("M-m", windows W.focusMaster)  -- Move focus to the master window
  , ("M-j", windows W.focusDown)    -- Move focus to the next window
  , ("M-k", windows W.focusUp)      -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
  , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
  , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
  , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
  , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack
  , ("M1-<Tab>", spawn "rofi -modi window -show window")
  , ("<XF86MonBrightnessDown>", spawn "sudo xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "sudo xbacklight -inc 10")
  -- Extra
  , ("C-M1-<Delete>", spawn "rofi -show powermenu -modi powermenu:rofi-power-menu")
  , ("M-S-<Space>", spawn "rofi -show kbd -modi kbd:rofi-switch-kbd-layout")
  , ("M-c", spawn "rofi -show calc -modi calc -no-show-match -no-sort")
  , ("M-x", spawn "rofi-keepassxc -d ~/Dropbox/Documents/caderno.kdbx")

  --, ("M-x", passPrompt vctXPromptConfig)
  --, ((modMask x , xK_p)                              , passPrompt xpconfig)
  --, ((modMask x .|. controlMask, xK_p)               , passGeneratePrompt xpconfig)
  --, ((modMask x .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt xpconfig)
  ]

-- Log Hooks
--
-- xmonad calls the =logHook= with every internal state update, which is useful
-- for (among other things) outputting status information to an external status
-- bar program such as xmobar or polybar.

-- Transparency to inactive windows
--
-- However, there is a neater way to do this: make the unfocused (inactive)
-- windows transparent. However, in order to do this, you must install a
-- compositor, like Compton of xcompmgr. For now I'm sticking to
-- [[https://github.com/chjj/compton][compton]], by eventually I'll try
-- [[https://github.com/yshui/picom][picom]], which is a fork being activelly
-- maintained.
vctTransparentInactive :: X()
vctTransparentInactive = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.7

-- Log hook for polybar
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-- Combine hooks
vctLogHook h = vctTransparentInactive <+> (dynamicLogWithPP (myLogHook h)) <+> historyHook

-- Manage Hooks
--
-- Send applications to the right workspace
vctManageHook :: ManageHook
vctManageHook = composeAll
    [ --manageDocks 
    className =? "Firefox" --> doShift "1:WWW"
    ]

-- Layouts
--
-- The Tall layout has the master pane on the left, taking half of the screen.
-- All other windows share the right half of the screen, and are stacked
-- vertically, top to bottom.
--
-- This my the go-to layout, meant to be the first one that you get when running
-- xmonad. It's common to have one window in focus while a couple secondary
-- windows are in view, so the Tall layout works great. It's very useful in many
-- situations, but the windows on the right start to feel a little crowded
-- beyond five windows.
vctLayoutTall = Tall 1 (3/100) (7/10)

-- Layout mirror
--
-- Mirrored version of =Tall=.
vctLayoutMirror = Mirror (Tall 1 (3/100) (3/5))

-- Create hook
vctLayoutHook = renamed [CutWordsLeft 1] $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ toggleLayouts Full vctLayoutTall ||| vctLayoutMirror ||| Full

-- Startup hook
--
-- These are commands we want XMonad to execute on startup or is restarted with
-- =mod-q=.
vctStartupHook :: X()
vctStartupHook = do
  -- Set wallpaper
  spawnOnce "~/.fehbg"
  -- Set cursor
  -- spawnOnce "xsetroot -cursor_name left_ptr"
  setDefaultCursor xC_left_ptr
  -- Use caps as an additional Ctrl (useful for emacs)
  -- spawnOnce "setxkbmap -layout br -option altwin:meta_alt -option ctrl:nocaps &" -- ABNT2 Layout
  spawnOnce "setxkbmap -layout br" -- ABNT2 Layout
  -- spawnOnce "setxkbmap us -variant intl &" -- ABNT2 Layout
  -- Compositing
  spawnOnce "picom --experimental-backend"
  -- Notifications
  spawnOnce "dunst"
  -- Start tmux in server mode
  spawnOnce "tmux new-session -d -s vct"
  -- Start Emacs in server mode
  spawnOnce "emacs --daemon"
  -- Start clipboard manager
  spawnOnce "klipper"
  -- Start dropbox
  spawnOnce "dropbox start"
  -- Start screensaver daemon
  spawnOnce "xscreensaver -no-splash &"
  -- Password Manager
  spawnOnce "keepassxc"
  -- Web browser
  spawnOnce "firefox"
  -- Show notification in the end
  spawn "notify-send -i \"emblem-important-symbolic\" \"XMonad started\""
  -- Start Polybar & stalonetray
  spawn "~/.config/polybar/launch.sh"
  -- Start Redshift
  --spawnOnce "redshift"


-- Run XMonad
--
-- Now we run xmonad with all the settings we defined previously:
main :: IO()
main = do
  home <- getHomeDirectory
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ docks
    $ desktopConfig
    { modMask = vctModMask
    , terminal = vctTerminal
    , borderWidth        = vctBorderWidth
    , workspaces         = vctWorkspaces
    , normalBorderColor  = vctNormalBorderColor
    , focusedBorderColor = vctFocusedBorderColor
    , manageHook         = vctManageHook <+> manageDocks <+> manageSpawn <+> manageHook desktopConfig
    , layoutHook         = avoidStruts $ vctLayoutHook
    , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig
    , startupHook        = vctStartupHook
    , logHook = workspaceHistoryHook <+> (vctLogHook dbus)
    } `additionalKeysP` vctKeys home
