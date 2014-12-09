{-# LANGUAGE OverloadedStrings #-}
-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.

import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import System.Taffybar.XMonadLog ( dbusLog)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- For dealing with Java.
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe, runProcessWithInput)

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

-- For xrandr info processing.
import qualified Data.Char as C
import Data.List

import System.IO
import System.FilePath.Posix
import System.Posix.Unistd -- to get hostname

-- For extra workspace nonsense
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------------------------------------
------------------------------------------------ MAIN --------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    dbus <- D.connectSession
    getWellKnownName dbus

    -- Resolution info
    xrandr   <- runProcessWithInput "xrandr" [] ""
    let dims =  screeninfo xrandr

    -- Conky background status info.
    conkyPID  <- spawnPID $ conky "config" "/home/amichaud/.config"

    -- Config xmonad.
    xmonad $ defaultConfig

        -- Hooks.
        { manageHook = mHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook    = dynamicLogWithPP (prettyPrinter dbus)

        -- Handles Java
        , startupHook = setWMName "LG3D"

        -- Stuff.
        , modMask    = mMask
        , workspaces = spaces
        , terminal   = term

        -- Mice are for squares.
        , focusFollowsMouse = False

        -- Border jazz
        , focusedBorderColor = focBord
        , normalBorderColor  = normBord
        , borderWidth        = bordWidth

        } `additionalKeys` keybinds "/home/amichaud/.config" host

---------------------------------------------
--------------- VARIABLES AND STUFF
---------------------------------------------

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

-- Font variables.
-- _ :: String
fontName  = "Dejavu"
font size = "-*-" ++ fontName ++ "-medium-r-normal-*-" ++ show size ++ "-140-75-75-p-74-iso10646-1"
fn size   = " -fn " ++ font size

-- XMonad colors and styling.
-- _ :: String
normFG   = base3
normBG   = base03
currBG   = orange
urgBG    = red
focBord  = orange
normBord = base1
bordWidth :: Dimension
bordWidth = 2

-- More complicated variables.

-- Keybinds, depending on host for audio keys.
keybinds configHome host =
    [
    -- Screen lock
      ((mMask .|. shiftMask,   xK_l),                     spawn "xscreensaver-command -lock")

    -- Backlight.
    , ((0,                     xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                     xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")

    -- Screenshots.
    , ((0,                     xK_Print),                 spawn $ scrot "")
    , ((controlMask,           xK_Print),                 spawn $ scrot "select")
    , ((shiftMask,             xK_Print),                 spawn $ scrot "delay")

    -- Recompile/restart XMonad. Modified to kill taskbar programs.
    , ((mMask,                 xK_q),                     spawn $ "xmonad --recompile;" ++
                                                                    "killall conky;" ++
                                                                    "xmonad --restart")
    -- Run DMenu.
    , ((mMask,                 xK_p),                     spawn $ "dmenu_run")

    -- Quick program spawns.
    , ((mMask,                 xK_a),                     spawn $ namedCmd "alsamixer" "")
    -- TODO- Find a cleaner way to do this.
    , ((mMask,                 xK_o),                     spawn $ namedCmd "ncmpcpp" ("-c " ++
                                                                     configHome ++ "/ncmpcpp/config"))
    , ((mMask,                 xK_b),                     spawn  "x-www-browser")

    , ((mMask .|. controlMask, xK_a),                     spawn $ term ++
                                                                " --title=__SSHAGENT" ++
                                                                " --command=\"bash -c '" ++
                                                                "ssh-add; read -p \\\"Press any key...\\\"" ++
                                                                "'\"")

    -- Various useful scripts.
    , ((mMask .|. shiftMask,   xK_s),                     spawn "~/bin/setWallpaper")
    , ((mMask,                 xK_n),                     spawn "~/bin/toggleOneko")

    -- Clip password with dzen and a nifty script.
    , ((mMask .|. shiftMask,   xK_p),                     spawn $ "~/bin/menu pass")
    -- Browse videos nicely.
    , ((mMask,                 xK_v),                     spawn $ "~/bin/menu vid")
    -- Browse playlists nicely.
    , ((mMask .|. shiftMask,   xK_o),                     spawn $ "~/bin/menu music")

    ] ++

    -- Audio keys.
    audioKeys host ++

    -- Keys for extra workspaces
    [((mMask .|. shiftMask, k), windows $ W.shift i) |
        (i, k) <- zip spaces wsKeys] ++

    [( (mMask,              k), windows $ W.greedyView i) |
        (i, k) <- zip spaces wsKeys]
        where wsKeys  = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

-------------------------------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF----------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------

-- Pretty printing.
myDzenPP :: String -> Handle -> PP
myDzenPP configHome h = dzenPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = pad
    , ppHidden  = pad . take 1
    , ppUrgent  = pad
    , ppSep     = " "
    , ppTitle   = pad . shorten 70
    }

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }
getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
conky file config = "conky --config=" ++ config ++ "/conky/" ++ file

-- Status bar dimensions.
statusHeight = 16

-- Terminal commands
term              = "xfce4-terminal --hide-menubar --show-borders"
termCmd cmd       = term ++ " --command=" ++ cmd
namedCmd cmd args = term ++ " --title=" ++ name ++ " --command='" ++ cmd ++ " " ++ args ++ "'"
    where name = "__" ++ map C.toUpper cmd

-- Use Windows key as mod key, it's convenient.
mMask = mod4Mask

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

--------------------------------------------------------------------------------------------------------------
-------------------------------------UTILITY--FUNCTIONS-------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- Grab resolution and positioning for each active monitor given xrandr string.
screeninfo :: String -> [[Int]]
screeninfo input = infoints
    where reslines   = filter (\x -> head (tail x ) == "connected" ) $ map words (lines input)
          resstrings = map (map (\x -> if C.isNumber x then x else ' ') . (!!2)) reslines
          info       = map words resstrings
          infoints   = map (map read) info :: [[Int]]

--------------------------------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS--------------------------------------------
--------------------------------------------------------------------------------------------------------------

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

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
lHook :: String -> Handle -> X ()
lHook config_home h = dynamicLogWithPP (myDzenPP config_home h)

------------------------------------------------------------------------------------------
----------------------------- END CUSTOM HOOKS -------------------------------------------
------------------------------------------------------------------------------------------
