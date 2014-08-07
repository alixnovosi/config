-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.

import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- For dealing with Java.
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

import Data.Char
import Data.List

import System.IO
import System.Posix.Unistd -- to get hostname
import System.Environment(getEnv)

-- For extra workspace nonsense
import qualified XMonad.StackSet as W

------------------------------------------------------------------------------------------
-----------------------------VARIABLES AND STUFF------------------------------------------
------------------------------------------------------------------------------------------

-- Color variables.
solarizedBase03  = "#002b36"
solarizedBase02  = "#073642"
solarizedBase01  = "#586e75"
solarizedBase00  = "#657b83"
solarizedBase0   = "#839496"
solarizedBase1   = "#93a1a1"
solarizedBase2   = "#eee8d5"
solarizedBase3   = "#fdf6e3"
solarizedYellow  = "#b58900"
solarizedOrange  = "#cb4b16"
solarizedRed     = "#dc322f"
solarizedMagenta = "#d33682"
solarizedViolet  = "#6c71c4"
solarizedBlue    = "#268bd2"
solarizedCyan    = "#2aa198"
solarizedGreen   = "#859900"


home :: String
home = "/home/amichaud/.xmonad"

-- Use XFCE4 terminal without menubar
myTerminal :: String
myTerminal = "xfce4-terminal --hide-menubar"

terminalCommand :: String -> String
terminalCommand cmd = myTerminal ++ " -x " ++ cmd

namedCmd :: String -> String
namedCmd cmd = myTerminal ++ " --title=" ++ name ++ " -x " ++ cmd ++ " "
    where name = "__" ++ map toUpper cmd

-- ModMask = windowsKey
myModMask = mod4Mask

--Borders
myFocusedBorderColor :: String
myFocusedBorderColor = solarizedCyan

myNormalBorderColor :: String
myNormalBorderColor = solarizedBase1

myBorderWidth :: Dimension
myBorderWidth = 2

-- Workspace titles
myWorkspaces :: [String]
myWorkspaces = ["` dev",  "1 term", "2 web",  "3 chat", "4 ssh",
                "5 game", "6 vid",  "7 work", "8 work", "9 work",
                "0 etc",  "- perf", "= music"]

-- Workspace keys.
myKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

audioKeys host

    -- Laptop doesn't have proper media keys because Lenovo are dumb.
    -- So, do it manually.
    | host == "pascal" = [ ((shiftMask, xF86XK_AudioMute), spawn "mpc toggle")
                          , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
                          , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")] ++ common

    | host == "raven" =  [ ((0, xF86XK_AudioPlay), spawn "mpc toggle")
                          , ((0, xF86XK_AudioNext), spawn "mpc next")
                          , ((0, xF86XK_AudioPrev), spawn "mpc prev")] ++ common

    | otherwise        = []
        where common = [ ((0, xF86XK_AudioLowerVolume),         spawn "amixer set Master 2-")
                       , ((0, xF86XK_AudioRaiseVolume),         spawn "amixer set Master 2+")
                       , ((0, xF86XK_AudioMute),                spawn "amixer set Master toggle")]

------------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF---------------------------------------------
------------------------------------------------------------------------------------------

-- Left Dzen - xmonad info and window title.
myDzenStatus :: String -> String
myDzenStatus host
    | host == "pascal" = status "700"
    | host == "raven"  = status "960"
    | otherwise        = status "960"
        where status width = "dzen2 -x '0' -w '" ++ width ++ "' -ta 'l'" ++ myDzenStyle


-- Right Dzen - Runs conky config.
myDzenConky :: String -> String
myDzenConky host
    | host == "pascal" = status "700" "800"
    | host == "raven"  = status "960" "860"
    | otherwise        = status "960" "860"
        where status start width = conkyCmd ++ " | dzen2 " ++ style start width
              conkyCmd = "conky -c " ++ home ++ "/dzen/.conky_dzen"
              style start width = "-x '" ++ start ++ "' -w '" ++
                                  width ++ "' -ta 'r'" ++ myDzenStyle

-- Trayer - system tray.
myTrayer :: String
myTrayer = "trayer --edge top --align right " ++
           "--widthtype pixel --width 100 " ++
           "--expand true --SetDockType true --SetPartialStrut true " ++
           "--transparent true --tint " ++ bgTrayer ++ " --expand true " ++
           "--heighttype pixel --height 16 --padding 2"

-- Bitmaps used to represent current layout.
myBitmapsPath :: String
myBitmapsPath = home ++ "/dzen/bitmaps/"

normalFG :: String
normalFG = solarizedBase3

normalBG :: String
normalBG = solarizedBase03

currentFG :: String
currentFG = solarizedBase3

currentBG :: String
currentBG = solarizedOrange

urgentBG :: String
urgentBG = solarizedRed

bgTrayer :: String
bgTrayer = "0x" ++ tail solarizedBase03

-- Stuff common to both dzen bars.
myDzenStyle :: String
myDzenStyle = myDzenFont ++ " -h '16' -y '0'"

myFont :: String
myFont = "-*-Inconsolata-medium-r-normal-*-14-140-75-75-p-74-iso10646-1"

myDzenFont :: String
myDzenFont = " -fn '" ++ myFont ++ "'"

-- Pretty printing.
myDzenPP :: PP
myDzenPP  = dzenPP
    { ppCurrent = dzenColor currentFG currentBG . pad
    , ppHidden  = dzenColor currentFG normalBG . pad . take 1
    , ppUrgent  = dzenColor currentFG urgentBG . pad
    , ppSep     = "|"
    , ppTitle   = shorten 150 . dzenColor normalFG normalBG . pad
    , ppLayout  = \x -> case x of
                      "Tall"        -> wrapBitmap "rob/tall.xbm"
                      "Mirror Tall" -> wrapBitmap "rob/mtall.xbm"
                      "Full"        -> wrapBitmap "rob/full.xbm"
    }
    where wrapBitmap bitmap = "^p(4)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(4)"

------------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS--------------------------------------------
------------------------------------------------------------------------------------------

ewmhConfig = ewmh defaultConfig

-- Custom manage hook.
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Xfce4-notifyd"  --> doIgnore

    , title     =? "__NCMPCPP"      --> doCenterFloat
    , title     =? "__ALSAMIXER"    --> doCenterFloat
    , title     =? "__XMONADHS"     --> doCenterFloat
    , title     =? "__HTOP"         --> doShift "-:perf"

    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFullFloat
    , className =? "Steam"          --> doFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin"         --> doShift "3:chat"
    , title     =? "SKype"          --> doShift "3:chat"
    ]

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP myDzenPP { ppOutput = hPutStrLn h }

------------------------------------------------------------------------------------------
----------------------------- END CUSTOM HOOKS -------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
--------------------------------- MAIN ---------------------------------------------------
------------------------------------------------------------------------------------------

main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    -- Get useful environment variables.
    editor <- getEnv "EDITOR"

    -- Status bar programs.
    status <- spawnPipe $ myDzenStatus host
    conky <- spawnPipe $ myDzenConky host
    trayer <- spawnPipe myTrayer

    xmonad $ withUrgencyHook NoUrgencyHook ewmhConfig

        -- Hooks.
        { manageHook = manageDocks <+> myManageHook <+> manageHook ewmhConfig
        , layoutHook = avoidStruts $ smartBorders $ layoutHook ewmhConfig
        , logHook = myLogHook status

        -- Handles Java
        , startupHook = setWMName "LG3D"

        -- Stuff.
        , modMask = myModMask
        , workspaces = myWorkspaces
        , terminal = myTerminal

        -- Mice are for squares.
        , focusFollowsMouse = False

        -- Border jazz
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor  = myNormalBorderColor
        , borderWidth        = myBorderWidth

        } `additionalKeys`

        (

        [ ((mod4Mask .|. shiftMask, xK_l),  spawn "xscreensaver-command -lock")

        -- Backlight.
        , ((0, xF86XK_MonBrightnessDown),   spawn "xbacklight -dec 7")
        , ((0, xF86XK_MonBrightnessUp),     spawn "xbacklight -inc 7")

        -- Screenshots.
        , ((0, xK_Print),                   spawn "scrot")
        , ((controlMask, xK_Print),         spawn "sleep 0.2; scrot -s")

        -- Modified to kill taskbar programs.
        , ((myModMask, xK_q),               spawn $ "killall dzen2 conky trayer;" ++
                                                        "xmonad --recompile;" ++
                                                        "xmonad --restart")

        -- Alsamixer, ncmpcpp, quick spawn bindings.
        , ((myModMask, xK_a),               spawn $ namedCmd "alsamixer")
        -- Find a better way to do this.
        , ((myModMask, xK_o),               spawn $ (namedCmd "ncmpcpp") ++
                                                    "-c ~/.config/ncmpcpp/config")

        -- various utility scripts
        , ((myModMask .|. shiftMask, xK_s), spawn "~/bin/setWallpaper")] ++

        -- Audio keys.
        audioKeys host ++

        -- Keys for extra workspaces
        [( (myModMask .|. shiftMask, k), windows $ W.shift i) |
         (i, k) <- zip myWorkspaces myKeys] ++

        [( (myModMask, k),          windows $ W.greedyView i) |
         (i, k) <- zip myWorkspaces myKeys]
        )

