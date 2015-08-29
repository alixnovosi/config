---------------------------------------------------------------------------------------------------
-- AUTHOR:  Andrew Michaud                                                                       --
-- FILE:    xmonad.hs                                                                            --
-- PURPOSE: XMonad configuration file                                                            --
-- UPDATED: 2015-08-26                                                                           --
-- LICENSE: MIT/BSD                                                                              --
---------------------------------------------------------------------------------------------------
import qualified Data.Char as C         (toUpper)
import Data.Maybe                       (fromMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.Environment               (getEnvironment)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow      -- dwm-style window tagging
import XMonad.Actions.GridSelect
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers       (doCenterFloat, doFullFloat)
import XMonad.Hooks.SetWMName         -- For dealing with Java stuff.
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Util.EZConfig             (additionalKeys)
import qualified XMonad.StackSet as W -- for extra workspaces.

import Colors

---------------------------------------------------------------------------------------------------
-------------------------------------------  MAIN  ------------------------------------------------
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do

    -- Get environment to pull some XDG vars.
    env <- getEnvironment

    -- Config xmonad.
    xmonad $ pagerHints $ defaultConfig
        { manageHook = mHook <+> manageHook defaultConfig
        , layoutHook = lHook

        -- Handles Java, ewmh nonsense.
        , startupHook = ewmhDesktopsStartup <+> setWMName "LG3D"

        , logHook         = ewmhDesktopsLogHook
        , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+>
                            handleEventHook defaultConfig

        , modMask    = mod4Mask
        , workspaces = spaces
        , terminal   = term

        , focusFollowsMouse = False -- Mice are for squares.
        , clickJustFocuses  = False -- Focusing click passed to window.

        -- Border jazz.
        , focusedBorderColor = base2
        , normalBorderColor  = base03
        , borderWidth        = 2

        } `additionalKeys` keybinds env mod4Mask

---------------------------------------------------------------------------------------------------
-------------------------------------------  KEYBINDS  --------------------------------------------
---------------------------------------------------------------------------------------------------
-- Keybinds, depending on host for audio keys.
keybinds env mask =
    -- Killing a window only removes it from current workspace.
    [ ((mask .|. shiftMask, xK_c), kill1)
    , ((mask,               xK_x), goToSelected defaultGSConfig)

    -- Cycle/shift workspaces.
    , ((mask .|. controlMask,               xK_h), prevWS)
    , ((mask .|. controlMask,               xK_l), nextWS)
    , ((mask .|. controlMask .|. shiftMask, xK_h), shiftToPrev)
    , ((mask .|. controlMask .|. shiftMask, xK_l), shiftToNext)

    -- Commands.
    , ((0,                    xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                    xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")
    , ((mask .|. shiftMask,   xK_l),                     spawn "xscreensaver-command -lock")
    , ((mask .|. shiftMask,   xK_d),                     spawn "xscreensaver-command -deactivate")
    -- Restart taffybar.
    , ((mask .|. controlMask, xK_q),                     spawn "pkill taffybar && taffybar")

    -- Screenshots.
    , ((0,           xK_Print), spawn $ scrot picHome "")
    , ((controlMask, xK_Print), spawn $ scrot picHome "select")
    , ((shiftMask,   xK_Print), spawn $ scrot picHome"delay")

    -- Quick program spawns.
    , ((mask,                 xK_a), spawn $ namedCmd "alsamixer" "")
    , ((mask,                 xK_o), spawn $ namedCmd "ncmpcpp" "")
    , ((mask,                 xK_b), spawn "x-www-browser")
    , ((mask .|. controlMask, xK_a), spawn $ namedCmd "ssh-add" "")

    -- Various useful scripts, which are also in my config repo.
    , ((mask,               xK_s), spawn $ binHome ++ "setWallpaper")
    , ((mask,               xK_n), spawn $ binHome ++ "toggleOneko")
    , ((mask .|. shiftMask, xK_p), spawn $ binHome ++ "menu pass")
    , ((mask,               xK_v), spawn $ binHome ++ "menu vid")
    , ((mask .|. shiftMask, xK_o), spawn $ binHome ++ "menu music")
    ] ++

    -- Audio keys.
    audioKeys host ++

    -- mod-<N> switches to workspace N.
    -- mod-shift-<N> moves to workspace N.
    -- mod-control-shift-<N> copies to workspace N.
    [((mask .|. m, k), windows $ f i)
        | (i, k) <- zip spaces wsKeys
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]

        where wsKeys  = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

              -- Determine xdg_data_home to grab scripts correctly.
              -- You may store your scripts elsewhere and want to change this.
              binHome = fromMaybe "~/.local/share" (lookup "XDG_DATA_HOME"    env) ++ "/bin"
              picHome = fromMaybe "~/pictures"     (lookup "XDG_PICTURES_DIR" env)

---------------------------------------------------------------------------------------------------
--------------------------------------------  OTHER  ----------------------------------------------
---------------------------------------------------------------------------------------------------
-- Terminal commands.
term              = "xfce4-terminal --hide-menubar --show-borders"
cmd cmd           = term ++ " --title=" ++ name ++ " --command='" ++ cmd ++ "'"
    where name = "__" ++ map C.toUpper $ head $ words cmd

-- Workspace titles.
spaces :: [String]
spaces = ["` term", "1 term", "2 socl", "3 socl", "4 play", "5 play", "6 play", "7 work", "8 work",
          "9 work", "0 etc.", "- etc.", "= etc."]
spaces = concat [map (++ " term") ["`", "1"], map (++ " socl") ["2", "3"],
                 map (++ " play") ["4", "5", "6"], map (++ " work") ["7", "8", "9"],
                 map (++ " etc.") ["-", "-", "="]]
spaces = zipWith ++ concat [replicate 2 " term", replicate 2 " socl", replicate 3 " play",
                            replicate 3 " work", replicate 3 " etc."]
                    (["`"] ++ map show [1..9] ++ ["0", "-", "="])
spaces = zipWith ++ (concat $ concatMap (replicate 2) [" term", " socl"] ++
                              concatMap (replicate 3) [" play", " work", " etc."])
                    (["`"] ++ map show [1..9] ++ ["0", "-", "="])

-- Laptop doesn't have proper media keys because Lenovo are dumb.  So, do it manually.
-- Actually, why not define those silly media keys for any host?  No downside.
audioKeys = [ ((shiftMask, xF86XK_AudioMute),        spawn "mpc toggle")
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")
            , ((0,         xF86XK_AudioPlay),        spawn "mpc toggle")
            , ((0,         xF86XK_AudioNext),        spawn "mpc next")
            , ((0,         xF86XK_AudioPrev),        spawn "mpc prev")
            , ((0,         xF86XK_AudioMute),        spawn "/usr/bin/pulseaudio-ctl mute")
            , ((0,         xF86XK_AudioLowerVolume), spawn "/usr/bin/pulseaudio-ctl down")
            , ((0,         xF86XK_AudioRaiseVolume), spawn "/usr/bin/pulseaudio-ctl up")]

-- Previously mentioned screenshotting nonsense.
scrot :: String -> String -> String
scrot picHome t = case t of ""       -> scrotGen ""
                            "select" -> scrotGen "-s "
                            "delay"  -> scrotGen "--delay 5 "
          -- Year, month, day, width by height.
    where format      = "'%Y-%m-%d-%s_$wx$h.png' "
          destination = "-e 'mv $f " ++ picHome ++ "/screenshots/' "
          scrotGen s  = "scrot " ++ s ++ format ++ destination

---------------------------------------------------------------------------------------------------
------------------------------------------  CUSTOM HOOKS  -----------------------------------------
---------------------------------------------------------------------------------------------------
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = manageDocks <+> composeAll
    [ title     =? "__NCMPCPP"   --> doCenterFloat
    , title     =? "__ALSAMIXER" --> doCenterFloat
    , title     =? "__XMONADHS"  --> doCenterFloat
    , title     =? "__SSH-ADD"   --> doCenterFloat

    -- Handle gimp and mpv specially.
    , className =? "Gimp" --> doFloat
    , className =? "mpv"  --> doFullFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin" --> doShift "3:chat"
    ]

lHook = avoidStruts $ smartBorders layouts
    where layouts = tiled ||| Mirror tiled ||| Full
          tiled   = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 1/2
          delta   = 3/100
