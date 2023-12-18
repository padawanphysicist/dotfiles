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

-- Util
import XMonad.Util.SpawnOnce
import XMonad.Util.Run -- For spawn
import XMonad.Util.EZConfig -- For custom keys
import XMonad.Util.NamedActions
import XMonad.Util.Cursor

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks -- Automatically manage dock type programs (panel, mainly)
import XMonad.Hooks.DynamicLog -- For panel
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.FadeInactive -- Add transparency to inactive windows
import XMonad.Hooks.UrgencyHook

-- Layout
import XMonad.Layout.Spacing -- Add spacing (gaps) between windows
import XMonad.Layout.ToggleLayouts -- For toggling layouts
import XMonad.Layout.Grid -- For Grid layout
import XMonad.Layout.Renamed -- For renaming workspaces
import XMonad.Layout.NoBorders

-- Custom modules
import ColorScheme.ColorScheme
import Colors.Nord
import Bar.Polybar
import Notifications.Dunst

-- Custom Variables
--
-- Here I set some values for things like font, terminal and editor. With this I
-- only have to change the value here to make modifications globally.

-- Change prefix key
--
-- =modMask= lets you specify which modkey you want to use. The default is
-- =mod1Mask= ("left Alt"). You may also consider using =mod3Mask= ("right
-- Alt"), which does not conflict with emacs keybindings. I prefer the "windows
-- key", which is usually =mod4Mask=.
vctModMask :: KeyMask
vctModMask = mod4Mask

-- Choose colorscheme
vctColorScheme :: ColorScheme
vctColorScheme = nordColorScheme
vctFocusedBorderColor :: String
vctFocusedBorderColor = primary vctColorScheme

-- Width of Windows border
--
-- Change this to a value > 0 to have border:
vctBorderWidth :: Dimension
vctBorderWidth = 5 -- Issues with touchpad right click when > 0


vctLauncherRofi :: String
vctLauncherRofi = "rofi -show 'drun' -theme "++ name vctColorScheme ++""

vctLauncher :: String
vctLauncher = vctLauncherRofi

vctTerminalKonsole :: String
vctTerminalKonsole = "konsole -e tmux"

vctTerminalXterm :: String
vctTerminalXterm = "if [[ $(date +%H%M) -gt 0600 ]] && [[ $(date +%H%M) -lt 1800 ]]; then xrdb -merge $HOME/.Xresources.d/xterm-light.Xresources; else xrdb -merge $HOME/.Xresources.d/xterm-dark.Xresources; fi; xterm -e tmux"

vctTerminal :: String
vctTerminal = vctTerminalXterm

vctEditorEmacs :: String
vctEditorEmacs = "emacsclient -c"

vctEditor :: String
vctEditor = vctEditorEmacs

vctScreenSaver :: String
vctScreenSaver = "xscreensaver-command -activate"

-- Workspaces
vctWorkspaces :: [String]
vctWorkspaces = ["1:WWW", "2:DEV", "3:READ", "4:AUX", "5:EXTRA"]

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
vctLayoutHook = renamed [CutWordsLeft 1] $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ toggleLayouts Full vctLayoutTall ||| vctLayoutMirror ||| noBorders Full

-- Startup hook
--
-- These are commands we want XMonad to execute on startup or is restarted with
-- =mod-q=.
vctStartupHook :: X()
vctStartupHook = do
  spawn "killall stalonetray; stalonetray"
  -- Enable Notifications
  spawnOnce "dunst"
  -- Enable emacs
  spawnOnce "emacs --daemon"
  -- Set keyboard layout
  spawn "setxkbmap br -variant abnt2"
  -- Set wallpaper
  spawn "~/.fehbg"
  
  spawn "pkill polybar; polybar"
  -- Compositing
  spawnOnce "picom"
  -- Sound
  spawn "gentoo-pipewire-launcher restart"
  -- Show notification in the end
  spawn "notify-send -i \"emblem-important-symbolic\" \"XMonad started\""
  spawn "mpv /usr/share/sounds/Oxygen-Sys-Log-In-Short.ogg"
  -- Redshift
  spawn "killall redshift; sleep 5; redshift"

  spawn "pkill xscreensaver; sleep 2; xscreensaver -no-splash"
  -- Start Dropbox
  spawn "dropbox stop; sleep 5; dropbox start"

-- Keybindings
vctKeys :: String -> [([Char], X ())]
vctKeys home =
  -- XMonad
  [ ("M-q", spawn "xmonad --restart") -- Recompiles XMonad
  , ("M-S-q", io exitSuccess)  -- Exits XMonad
  -- Programs
  , ("M-p", spawn vctLauncher)
  , ("M-e", spawn vctEditor)
  --, ("M-z", sendMessage (Toggle "Full"))
  , ("M-S-l", spawn vctScreenSaver)
  --, ("M-f", spawn vctFileManager)
  ---- Increase/decrease spacing (gaps)
  --, ("M-d", decWindowSpacing 4)           -- Decrease window spacing
  --, ("M-i", incWindowSpacing 4)           -- Increase window spacing
  --, ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
  --, ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing
  ---- Kill windows
  --, ("M-S-c", kill1)     -- Kill the currently focused client
  --, ("M-S-a", killAll)   -- Kill all windows on current workspace
  ---- Windows navigation
  --, ("M-m", windows W.focusMaster)  -- Move focus to the master window
  --, ("M-j", windows W.focusDown)    -- Move focus to the next window
  --, ("M-k", windows W.focusUp)      -- Move focus to the prev window
  --, ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
  --, ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
  --, ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
  --, ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
  --, ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
  --, ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack
  --, ("M1-<Tab>", spawn "rofi -modi window -show window")
  , ("<XF86MonBrightnessDown>", spawn "screenlight down")
  , ("<XF86MonBrightnessUp>", spawn "screenlight up")
  ---- Extra
  --, ("C-M1-<Delete>", spawn "rofi -show powermenu -modi powermenu:rofi-power-menu")
  --, ("M-S-<Space>", spawn "rofi -show kbd -modi kbd:rofi-switch-kbd-layout")
  --, ("M-c", spawn "rofi -show calc -modi calc -no-show-match -no-sort")
  --, ("M-x", spawn "rofi-keepassxc -d ~/Dropbox/Documents/caderno.kdbx")
  --
  ----, ("M-x", passPrompt vctXPromptConfig)
  ----, ((modMask x , xK_p)                              , passPrompt xpconfig)
  ----, ((modMask x .|. controlMask, xK_p)               , passGeneratePrompt xpconfig)
  ----, ((modMask x .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt xpconfig)
  ]

-- Run XMonad
--
-- Now we run xmonad with all the settings we defined previously:
main :: IO ()
main = do
  home <- getHomeDirectory
  -- vctCreatePolybarConfig vctColorScheme
  -- vctCreateDunstConfig vctColorScheme
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ docks
    $ desktopConfig
    { modMask = vctModMask
    , terminal = vctTerminal
    , borderWidth = vctBorderWidth
    , workspaces         = vctWorkspaces
    , focusedBorderColor = vctFocusedBorderColor
    , layoutHook         = avoidStruts $ vctLayoutHook
    , startupHook        = vctStartupHook
    } `additionalKeysP` vctKeys home
