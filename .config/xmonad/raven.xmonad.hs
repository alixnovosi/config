-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints

import XMonad.Actions.SpawnOn

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.ICCCMFocus

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

import XMonad.Hooks.FadeInactive

import System.IO

-- Environment variables
import System.Environment(getEnv)

-- For extra workspace nonsense
import qualified XMonad.StackSet as W

-- For dealing with Java.
import XMonad.Hooks.SetWMName

-----------------------------------------------------------------------------------------
-----------------------------VARIABLES AND STUFF-----------------------------------------
-----------------------------------------------------------------------------------------

-- Woo XDG base spec.
xdg_config_home :: String
xdg_config_home <- getenv("XDG_CONFIG_HOME")

xmonad_home :: String
xmonad_home = xdg_config_home ++ "/xmonad"

-- Use XFCE4 terminal without menubar
myTerminal :: String
myTerminal = "xfce4-terminal --hide-menubar"

terminalCommand :: String -> String
terminalCommand cmd = myTerminal ++ " -x " ++ cmd

-- ModMask = windowsKey
myModMask = mod4Mask

--Borders
myFocusedBorderColor :: String
myFocusedBorderColor = "turquoise"

myNormalBorderColor :: String
myNormalBorderColor = "#333333"

myBorderWidth :: Dimension
myBorderWidth = 2

-- Workspace titles
myWorkspaces :: [String]
myWorkspaces = ["` dev",  "1 term", "2 web",  "3 chat", "4 ssh",
                "5 game", "6 vid",  "7 work", "8 work", "9 work",
                "0 etc",  "- perf", "= music"]

-- Workspace keys.
myKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

-----------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF--------------------------------------------
-----------------------------------------------------------------------------------------

-- Forward window information to dzen bar, formatted.
myLogHook :: Handle -> X ()
myLogHook h = fadeInactiveLogHook (1.0) <+> (dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h })

-- Status bars
-- Dzen handles window info and the right is conky being cool.
myDzenStatus :: String
myDzenStatus = "dzen2 -x '0' -w '800' -ta 'l'" ++ myDzenStyle

myDzenConky :: String
myDzenConky  = "conky -c ~/.conky_dzen | dzen2 -x '800' -w '700' -ta 'r'" ++ myDzenStyle

myDzenFont :: String
-- fndry-fmly-wght-slant-sWdth-adstyl-pxlsz-ptsz-resx-resy-spc-avgWidth-rgstry-encdng
myDzenFont = " -fn '-*-verdana-medium-r-normal-*-*-100-*-*-p-*-iso10646-1' "

myDzenStyle :: String
myDzenStyle = myDzenFont ++ "-h '16' -y '0' -fg '#ffffff' -bg " ++ bgDzen

bgDzen :: String
bgDzen = "'#383a3b'"

myDzenFont :: String
-- fndry-fmly-wght-slant-sWdth-adstyl-pxlsz-ptsz-resx-resy-spc-avgWidth-rgstry-encdng
myDzenFont = " -fn '-*-verdana-medium-r-normal-*-*-100-*-*-p-*-iso10646-1' "

bgXmonad :: String
bgXmonad = "#383a3b"

-- Pretty printing.
myDzenPP :: PP
myDzenPP  = dzenPP
    { ppCurrent = dzenColor bgXmonad "#gray" . pad
    , ppHidden  = dzenColor "gray" "" . pad . take 1
    , ppUrgent  = dzenColor "#ff0000" "" . pad
    , ppSep     = "|"
    , ppTitle   = shorten 150 . dzenColor "white" "" . pad
    , ppLayout  = \x -> case x of
                      "Tall"        -> wrapBitmap "rob/tall.xbm"
                      "Mirror Tall" -> wrapBitmap "rob/mtall.xbm"
                      "Full"        -> wrapBitmap "rob/full.xbm"
    }
    where wrapBitmap bitmap = "^p(8)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(8)"

-----------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS-------------------------------------------
-----------------------------------------------------------------------------------------

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

    -- Chat windows go to workspace 3
    , className =? "Pidgin"         --> doShift "3:chat"
    , title     =? "SKype"          --> doShift "3:chat"
    ]

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP myDzenPP { ppOutput = hPutStrLn h }

-- END CUSTOM HOOKS --

-- Main.
main :: IO ()
main = do

    -- Status bar programs.
    status <- spawnPipe myDzenStatus
    conky  <- spawnPipe myDzenConky
    trayer <- spawnPipe myTrayer

    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig

        -- Hooks.
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook = myLogHook status

        -- Handles Java.
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

        ([ ((mod4Mask .|. shiftMask, xK_l),      spawn "xscreensaver-command -lock")

        -- Volume.
        , ((0, xF86XK_AudioLowerVolume),         spawn "amixer set Master 2-")
        , ((0, xF86XK_AudioRaiseVolume),         spawn "amixer set Master 2+")
        , ((0, xF86XK_AudioMute),                spawn "amixer set Master toggle")

        -- Music controls. Should be on relevant xF86 keys, but my keyboard doesn't
        -- have those.
        , ((0, xF86XK_AudioPlay),                spawn "mpc toggle")
        , ((0, xF86_AudioNext),                  spawn "mpc next")
        , ((0, xF86XK_AudioPlay),                spawn "mpc prev")

        -- Backlight.
        , ((0, xF86XK_MonBrightnessDown),        spawn "xbacklight -dec 7")
        , ((0, xF86XK_MonBrightnessUp),          spawn "xbacklight -inc 7")

        -- Screenshots.
        , ((0, xK_Print),                        spawn "scrot")
        , ((controlMask, xK_Print),              spawn "sleep 0.2; scrot -s")

        -- Modified to kill taskbar programs.
        , ((myModMask, xK_q),                    spawn $ "killall dzen2 conky;" ++
                                                       " xmonad --recompile;" ++
                                                       " xmonad --restart")
        -- Alsamixer, ncmpcpp, xmonad.hs quick spawn bindings.
        , ((myModMask, xK_a),                    spawn $ myTerminal ++
                                                       " --title=__ALSAMIXER -x alsamixer")
        , ((myModMask, xK_o),                    spawn $ myTerminal ++
                                                       " --title=__NCMPCPP -x ncmpcpp")
        , ((myModMask .|. controlMask, xK_x),    spawn $ myTerminal ++
                                                       " --title=__XMONADHS -x" ++
                                                       " vim ~/.xmonad/xmonad.hs")

        -- various utility scripts
        , ((myModMask .|. shiftMask, xK_s),      spawn "~/bin/setWallpaper")

        ] ++

        -- Keys for extra workspaces
        [( (myModMask .|. shiftMask, k), windows $ W.shift i) |
         (i, k) <- zip myWorkspaces myKeys] ++

        [( (myModMask, k),          windows $ W.greedyView i) |
         (i, k) <- zip myWorkspaces myKeys])

