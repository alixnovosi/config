{-# LANGUAGE OverloadedStrings #-}
-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName -- For dealing with Java stuff.

import XMonad.Layout.NoBorders (smartBorders)

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, runProcessWithInput)

import qualified XMonad.StackSet as W -- for extra workspaces.

-- DBus (which Taffybar uses)
import qualified DBus as D
import qualified DBus.Client as D

import qualified Codec.Binary.UTF8.String as UTF8

import System.Taffybar.XMonadLog (dbusLog)

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Char as C (toUpper)

import System.Posix.Unistd (nodeName, getSystemID)

-----------------------------------------------------------------------------
-------------------------------- MAIN ---------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    dbus <- D.connectSession
    getWellKnownName dbus

    -- Conky background status info.
    conkyPID  <- spawnPID "conky --config=$HOME/.config/conky/config"

    -- Config xmonad.
    xmonad $ defaultConfig

        -- Hooks.
        { manageHook = mHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook    = dynamicLogWithPP (prettyPrinter dbus)

        -- Handles Java
        , startupHook = setWMName "LG3D"

        -- Stuff.
        , modMask    = mod4Mask
        , workspaces = spaces
        , terminal   = term

        -- Mice are for squares.
        , focusFollowsMouse = False

        -- Border jazz
        , focusedBorderColor = focBord
        , normalBorderColor  = normBord
        , borderWidth        = 2

        } `additionalKeys` keybinds host


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
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- XMonad colors and styling.
-- _ :: String
normFG   = base3
normBG   = base03
currBG   = orange
urgBG    = red
focBord  = orange
normBord = base1

---------------------------------------------------------------------------------------------------------------
-------------------------------------------------   KEYBINDS   ------------------------------------------------
---------------------------------------------------------------------------------------------------------------

-- Keybinds, depending on host for audio keys.
keybinds host =
    [
    -- Screen lock
      ((mod4Mask .|. shiftMask,   xK_l),                     spawn "xscreensaver-command -lock")

    -- Backlight.
    , ((0,                        xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                        xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")

    -- Screenshots.
    , ((0,                        xK_Print),                 spawn $ scrot "")
    , ((controlMask,              xK_Print),                 spawn $ scrot "select")
    , ((shiftMask,                xK_Print),                 spawn $ scrot "delay")

    -- Recompile/restart XMonad. Modified to kill taskbar programs.
    , ((mod4Mask,                 xK_q),                     spawn $ "xmonad --recompile;" ++
                                                                    "killall conky;" ++
                                                                    "xmonad --restart")
    -- Run DMenu.
    , ((mod4Mask,                 xK_p),                     spawn "dmenu_run")

    -- Quick program spawns.
    , ((mod4Mask,                 xK_a),                     spawn $ namedCmd "alsamixer" "")
    -- TODO- Find a cleaner way to do this.
    , ((mod4Mask,                 xK_o),                     spawn $ namedCmd "ncmpcpp" "-c /home/amichaud/.config/ncmpcpp/config")
    , ((mod4Mask,                 xK_b),                     spawn "x-www-browser")

    , ((mod4Mask .|. controlMask, xK_a),                     spawn $ term ++
                                                                " --title=__SSHAGENT" ++
                                                                " --command=\"bash -c '" ++
                                                                "ssh-add; read -p \\\"Press any key...\\\"" ++
                                                                "'\"")

    -- Various useful scripts.
    , ((mod4Mask .|. shiftMask,   xK_s),                     spawn "~/bin/setWallpaper")
    , ((mod4Mask,                 xK_n),                     spawn "~/bin/toggleOneko")

    -- Clip password with dzen and a nifty script.
    , ((mod4Mask .|. shiftMask,   xK_p),                     spawn "~/bin/menu pass")
    -- Browse videos nicely.
    , ((mod4Mask,                 xK_v),                     spawn "~/bin/menu vid")
    -- Browse playlists nicely.
    , ((mod4Mask .|. shiftMask,   xK_o),                     spawn "~/bin/menu music")

    ] ++

    -- Audio keys.
    audioKeys host ++

    -- Keys for extra workspaces
    [((mod4Mask .|. shiftMask, k), windows $ W.shift i) |
        (i, k) <- zip spaces wsKeys] ++

    [( (mod4Mask,              k), windows $ W.greedyView i) |
        (i, k) <- zip spaces wsKeys]
        where wsKeys  = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

-----------------------------------------------------------------------------------
------------------------------    STATUS BAR STUFF  -------------------------------
-----------------------------------------------------------------------------------

-- This section from alexkay on github, modified by me.
prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoColor normFG normBG . pangoSanitize . shorten 70
    , ppCurrent  = pangoColor normBG orange . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor normBG cyan . wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor normFG normBG . take 1
    , ppUrgent   = pangoColor normBG red
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

-- Get DBus output.  Not my code.
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ UTF8.decodeString str ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String -> String
pangoColor fg bg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\" background=\"" ++ bg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

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
spaces = ["` sys", "1 sys", "2 web",  "3 chat", "4 ssh", "5 play", "6 play",
          "7 work", "8 work", "9 work", "0 etc",  "- \956sic", "= perf"]

-- Audio control keys.
-- Assuming computer has the proper audio keys if it's not my laptop.
audioKeys = \h -> case h of

    -- Laptop doesn't have proper media keys because Lenovo are dumb.
    -- So, do it manually.
    "pascal" -> [ ((shiftMask, xF86XK_AudioMute),        spawn "mpc toggle")
                , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
                , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")] ++ common

    _        -> [ ((0, xF86XK_AudioPlay), spawn "mpc toggle")
                , ((0, xF86XK_AudioNext), spawn "mpc next")
                , ((0, xF86XK_AudioPrev), spawn "mpc prev")] ++ common

    where common = [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 2%-")
                   , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 2%+")
                   , ((0, xF86XK_AudioMute),        spawn "amixer -D pulse sset Master toggle")]

-- Screenshot commands
scrot :: String -> String
scrot = \x -> case x of
                  ""       -> "scrot " ++ format ++ destination
                  "select" -> "scrot " ++ "-s " ++ format ++ destination
                  "delay"  -> "scrot " ++ "--delay 5 " ++ format ++ destination
    where format      = "'%d-%m-%Y-%s_$wx$h.png' "
          destination = "-e 'mv $f ~/pictures/screenshots/' "

-------------------------------------------------------------------
-------------------------   CUSTOM HOOKS   ------------------------
-------------------------------------------------------------------

-- Custom urgency hook.
uHook = dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }

-- Custom manage hook.
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = manageDocks <+> composeAll
    [ className =? "Xfce4-notifyd"  --> doIgnore

    -- Float special command windows.
    , title     =? "__NCMPCPP"      --> doCenterFloat
    , title     =? "__ALSAMIXER"    --> doCenterFloat
    , title     =? "__XMONADHS"     --> doCenterFloat
    , title     =? "__SSHAGENT"     --> doCenterFloat
    , title     =? "__HTOP"         --> doShift "-:perf"

    -- Handle gimp and mpv specially.
    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFullFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin"         --> doShift "3:chat"
    , title     =? "Skype"          --> doShift "3:chat"
    ] <+> manageHook defaultConfig
