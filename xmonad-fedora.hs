-- PrDE XMonad Config

import Control.Monad
import Data.Maybe (maybe)
import System.Posix.Env (getEnv, putEnv)

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Layout.Maximize
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified Data.Map as M
import qualified XMonad.StackSet as W


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)
    , ((modm,xK_Tab), windows W.focusDown)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)

    -- Launch Emacs client
    , ((modm, xK_e), spawn "emacsclient -c")

    -- Launch browser
    , ((modm, xK_f), spawn "$(grep '^Exec' $(locate $(xdg-mime query default x-scheme-handler/http)) | tail -1 | sed 's/^Exec=//' | sed 's/%.//')")

    , ((modm, xK_Left), prevWS)
    , ((modm, xK_Right), nextWS)

    --, ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Maximize the focused window temporarily
    , ((modm, xK_equal), withFocused $ sendMessage . maximizeRestore)

    -- Push window back into tiling
    , ((modm, xK_minus), withFocused $ windows . W.sink)

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_Left), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_Right), sendMessage Expand)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Restart xmonad
    , ((modm, xK_q), restart "xmonad" True)

    , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    ]
    ++

    [((m .|. modm, k), windows $ f i)    
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


main = do
     session <- getEnv "DESKTOP_SESSION"
     putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
     emacs <- spawnPipe "/usr/bin/emacs --daemon"
     conky <- spawnPipe "[ $(/usr/bin/pidof conky) ] || /usr/bin/conky --daemonize"
     xmonad  $ gnomeConfig  -- maybe desktopConfig desktop session
        {
        modMask = mod4Mask
        , focusFollowsMouse = True
        , focusedBorderColor = "green"
        , normalBorderColor = "#444"
        , keys = myKeys
        , terminal = "gnome-terminal"
        , handleEventHook = serverModeEventHook
        }

desktop "gnome" = gnomeConfig
desktop "kde" = kde4Config
desktop "xfce" = xfceConfig
desktop "xmonad-mate" = gnomeConfig
desktop _ = desktopConfig
