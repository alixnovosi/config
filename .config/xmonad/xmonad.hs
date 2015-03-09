-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat)
import XMonad.Hooks.SetWMName -- For dealing with Java stuff.
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W -- for extra workspaces.

import qualified Data.Char as C (toUpper)
import Graphics.X11.ExtraTypes.XF86
import System.Posix.Unistd (nodeName, getSystemID)
import System.Taffybar.Hooks.PagerHints (pagerHints)

-----------------------------------------------------------------------------
-------------------------------  MAIN  --------------------------------------
-----------------------------------------------------------------------------
main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    -- Config xmonad.
    xmonad $ ewmh $ pagerHints $ defaultConfig

        -- Hooks.
        { manageHook = mHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig

        -- Handles Java, ewmh nonsense
        , startupHook = ewmhDesktopsStartup <+> setWMName "LG3D"

        , logHook         = ewmhDesktopsLogHook
        , handleEventHook = ewmhDesktopsEventHook <+>
                            handleEventHook defaultConfig <+>
                            fullscreenEventHook

        , modMask    = mod4Mask
        , workspaces = spaces
        , terminal   = term

        , focusFollowsMouse = False -- Mice are for squares.
        , clickJustFocuses  = False -- Focusing click passed to window.

        -- Border jazz
        , focusedBorderColor = base2
        , normalBorderColor  = base03
        , borderWidth        = 2

        } `additionalKeys` keybinds host mod4Mask

---------------------------------------------------------------------
----------------------------- COLORS --------------------------------
---------------------------------------------------------------------
-- Color variables.
-- _ :: String
-- TODO put these in a common file somewhere and refer to them there.
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"

---------------------------------------------------------------------------------
--------------------------------   KEYBINDS   -----------------------------------
---------------------------------------------------------------------------------
-- Keybinds, depending on host for audio keys.
keybinds host mask =
    [
    -- Start/Halt xscreensaver
      ((mask .|. shiftMask,   xK_l), spawn "xscreensaver-command -lock")
    , ((mask .|. controlMask, xK_l), spawn "xscreensaver-command -deactivate")

    -- Backlight.
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0, xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")

    -- Screenshots.
    , ((0,           xK_Print), spawn $ scrot "")
    , ((controlMask, xK_Print), spawn $ scrot "select")
    , ((shiftMask,   xK_Print), spawn $ scrot "delay")

    -- Quick program spawns.
    , ((mask,                 xK_a), spawn $ namedCmd "alsamixer" "")
    -- TODO- Find a cleaner way to do this.
    , ((mask,                 xK_o), spawn $ namedCmd "ncmpcpp" "")
    , ((mask,                 xK_b), spawn "x-www-browser")

    , ((mask .|. controlMask, xK_a), spawn $ namedCmd "ssh-add" "")

    -- Various useful scripts.
    , ((mask .|. shiftMask, xK_s), spawn "setWallpaper")
    , ((mask,               xK_n), spawn ". ~/.zshrc; toggleOneko")
    -- dmenu script for passwords, videos, playlists
    , ((mask .|. shiftMask, xK_p), spawn ". ~/.zshrc; menu pass")
    , ((mask,               xK_v), spawn "menu vid")
    , ((mask .|. shiftMask, xK_o), spawn "menu music")
    ] ++

    -- Audio keys.
    audioKeys host ++

    -- Keys for extra workspaces
    [((mask .|. shiftMask, k), windows $ W.shift i) |
        (i, k) <- zip spaces wsKeys] ++

    [( (mod4Mask,              k), windows $ W.greedyView i) |
        (i, k) <- zip spaces wsKeys]
        where wsKeys  = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

----------------------------------------------------------------------------------------------
----------------------------------------    OTHER    -----------------------------------------
----------------------------------------------------------------------------------------------
-- Terminal commands
term              = "xfce4-terminal --hide-menubar --show-borders"
termCmd cmd       = term ++ " --command=" ++ cmd
namedCmd cmd args = term ++ " --title=" ++ name ++ " --command='" ++ cmd ++ " " ++ args ++ "'"
    where name = "__" ++ map C.toUpper cmd

-- Workspace titles
spaces :: [String]
spaces = ["` term", "1 term", "2 socl", "3 socl", "4 play", "5 play", "6 play", "7 work",
          "8 work", "9 work", "0 etc.", "- etc.", "= etc"]

-- Audio control keys.
-- Assuming computer has the proper audio keys if it's not my laptop.
audioKeys h = case h of

    -- Laptop doesn't have proper media keys because Lenovo are dumb.
    -- So, do it manually.
    "pascal" -> [ ((shiftMask, xF86XK_AudioMute),        spawn "mpc toggle")
                , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
                , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")
                , ((0,         xF86XK_AudioMute),        spawn "/usr/bin/pulseaudio-ctl mute")
                , ((0,         xF86XK_AudioRaiseVolume), spawn "/usr/bin/pulseaudio-ctl up")
                , ((0,         xF86XK_AudioLowerVolume), spawn "/usr/bin/pulseaudio-ctl down")]

    _        -> [ ((0, xF86XK_AudioPlay),        spawn "mpc toggle")
                , ((0, xF86XK_AudioNext),        spawn "mpc next")
                , ((0, xF86XK_AudioPrev),        spawn "mpc prev")
                , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q -D pulse sset Master 2%-")
                , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q -D pulse sset Master 2%+")
                , ((0, xF86XK_AudioMute),        spawn "amixer -q -D pulse sset Master toggle")]

-- Screenshot commands
scrot :: String -> String
scrot = \x -> case x of
                  ""       -> "scrot " ++ format ++ destination
                  "select" -> "scrot " ++ "-s " ++ format ++ destination
                  "delay"  -> "scrot " ++ "--delay 5 " ++ format ++ destination
    where format      = "'%d-%m-%Y-%s_$wx$h.png' "
          destination = "-e 'mv $f ~/pictures/screenshots/' "

--------------------------------------------------
----------------   CUSTOM HOOKS   ----------------
--------------------------------------------------
-- Custom manage hook.
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = manageDocks <+> composeAll
    [ className =? "Xfce4-notifyd" --> doIgnore

    -- Float special command windows.
    , title     =? "__NCMPCPP"   --> doCenterFloat
    , title     =? "__ALSAMIXER" --> doCenterFloat
    , title     =? "__XMONADHS"  --> doCenterFloat
    , title     =? "__SSH-ADD"   --> doCenterFloat

    -- Handle gimp and mpv specially.
    , className =? "Gimp" --> doFloat
    , className =? "mpv"  --> doFullFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin" --> doShift "3:chat"
    ] <+> manageHook defaultConfig
