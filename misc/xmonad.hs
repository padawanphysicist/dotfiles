import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M         -- haskell modules
import qualified XMonad.StackSet as W  -- xmonad core
import XMonad.Actions.FloatKeys        -- actions (keyResizeWindow)
import XMonad.Actions.FloatSnap        -- actions (snapMove)

vctModMask = mod4Mask -- Mod key is the Windows key
vctBorderWidth = 2  -- I really need this thick!
vctFocusedBorderColor = "#ff4500"
vctWorkspaces = ["1:WWW","2:DEV","3:AUX"]
vctTerminal = "xterm -e 'tmux attach -t santos'"
vctAdditionalKeys = [((vctModMask, xK_p), spawn "rofi -sidebar-mode -modi 'run,drun,ssh' -show 'run'"), ((vctModMask, xK_z), spawn "emacsclient -c"), ((0, xF86XK_MonBrightnessUp), spawn "sudo xbacklight -i 10"), ((0, xF86XK_MonBrightnessDown), spawn "sudo xbacklight -d 10")]

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Taken from here:
-- https://github.com/dodie/dotfiles/blob/263d1df4086d6b35d9c7192ddcc7f229d11eba11/xmonad/xmonad.symlink/xmonad.hs#L52
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Sink the window back to the grid.
    , ((modm, button2), (\w -> withFocused $ windows . W.sink))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
  {
    modMask = vctModMask
  , workspaces = vctWorkspaces
  , borderWidth = vctBorderWidth
  , mouseBindings = myMouseBindings
  , focusedBorderColor = vctFocusedBorderColor
    -- Run autostart script when initializing
  , startupHook = spawn "~/.autostart"
    -- All terminals should connect to the same tmux session
  , terminal = vctTerminal
  } `additionalKeys` vctAdditionalKeys
