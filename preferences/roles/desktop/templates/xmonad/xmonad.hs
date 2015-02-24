-- XMonad Configuration
-- Author: jianingy.yang@gmail.com

import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment)
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows (rotFocusedUp, rotFocusedDown)
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Magnifier
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.Gaps
-- import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run (safeSpawn)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- getIcon :: String -> IO String
-- getIcon out = do
--      hd <- getHomeDirectory
--      return $ "^i(" ++ hd ++ ".xmonad/dzen2/" ++ out

userHome :: String
userHome = "{{ ansible_env.HOME }}"

main :: IO()
main = do
--       xmonad $ ewmh defaultConfig
--       Must use gnomeConfig or we will encounter a timeout error on
--       registering xmond to gnome session
         xmonad $ ewmh gnomeConfig
            { modMask = mod4Mask
            , terminal = "urxvt -ls"
            , borderWidth = 1
            , focusFollowsMouse = False
            , keys = myKeys
            , workspaces = myWorkspaces
            , normalBorderColor = "#49483E"
            , focusedBorderColor = "#81a2be"
            , layoutHook = gaps [(L, 13), (U, 37), (R, 13), (D, 13)] $ lessBorders OnlyFloat $ avoidStruts $ myLayout
            , manageHook = myManageHook <+> manageHook defaultConfig
            , startupHook = gnomeRegisterUtopic >> startupHook gnomeConfig
            }

-- Urgency hint options:
-- Layouts:
myTabTheme :: Theme
myTabTheme = defaultTheme { decoHeight = 22
                   , activeColor = "#333333"
                   , inactiveColor = "#0D0D0D"
                   , activeBorderColor = "#4B5054"
                   , inactiveBorderColor = "#4B5054"
                   , activeTextColor = "#ebac54"
                   , inactiveTextColor = "#666666"
                   }

-- myWorkspaces :: [WorkspaceId]
-- myWorkspaces = ["MAIN"] ++ map show [2 .. 24]
myWorkspaces :: [[Char]]
myWorkspaces = ["Emacs", "Browser", "Devel", "Operation0", "Operation1", "Operation2", "Operation3", "Mail", "Rdesktop"]

--

myLayout = avoidStruts
    $ onWorkspace "Emacs" layout_toggle_devel
    $ onWorkspace "Browser" layout_toggle_browse
    $ onWorkspace "Devel" layout_toggle_devel
    $ onWorkspace "Operation0" layout_toggle_ops
    $ onWorkspace "Operation1" layout_toggle_ops
    $ onWorkspace "Operation2" layout_toggle_ops
    $ onWorkspace "Operation3" layout_toggle_ops
    $ onWorkspace "Mail" layout_toggle
    $ onWorkspace "Rdesktop" layout_rdesktop
    $ layout_toggle
  where

  -- layout specific variables

    -- basic information
    goldenRatio = 233 / 377
    magStep = toRational (1+goldenRatio)
    ratio12 = 1 / 2
    ratio_main = 8 / 18
    ratio15 = 1 / 5
    delta = 3 / 100
    defaultSpace = 1
    nmaster = 1

    -- basic layouts
    layout_grid = spacing defaultSpace $ Grid
    layout_tall_spread = noBorders $ spacing 13 $ Tall nmaster delta ratio12
    layout_tall = spacing defaultSpace $ Tall nmaster delta ratio12
    layout_mirror_tall = spacing defaultSpace $ Mirror $ Tall nmaster delta ratio12
    layout_circle = Circle
    layout_tabup = tabbed shrinkText myTabTheme
    -- layout_devel = spacing defaultSpace $ Tall nmaster delta ratio12
    layout_devel = spacing defaultSpace $ magnifier (Tall 1 (3/100) (1/2))

    layout_rdesktop = spacing defaultSpace $ Tall nmaster delta ratio15
    layout_browse = spacing defaultSpace $ Tall nmaster delta ratio15
    --  layout_tabs = (layout_tabup *//* layout_tabup)
    layout_magnify_grid = spacing defaultSpace $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Grid
    layout_magnify_circle = spacing defaultSpace $ windowArrange $ magnifiercz' magStep $ MT.mkToggle (REFLECTX ?? EOT) $ MT.mkToggle (REFLECTY ?? EOT) $ Circle

    -- cominbation layouts
    -- layout_trinity_www = spacing 3 $combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Google-chrome")
    -- layout_trinity_emacs = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "Emacs")
    -- layout_trinity_term = spacing 3 $ combineTwoP (TwoPane delta goldenRatio) (Full) (layout_tabs) (ClassName "URxvt")
    layout_trinity_column = spacing defaultSpace $ ThreeColMid nmaster delta ratio_main
    -- layout_toggle_trinity = toggleLayouts Full (layout_trinity_column ||| layout_trinity_www ||| layout_trinity_term ||| layout_trinity_emacs ||| Full)

    -- workspace layouts
    layout_toggle_devel = toggleLayouts Full (layout_tall ||| layout_devel)
    layout_toggle_browse = toggleLayouts Full (layout_browse ||| layout_magnify_grid ||| layout_tall)
    layout_toggle_ops = toggleLayouts Full (layout_grid ||| layout_trinity_column ||| layout_tall)

    layout_toggle = toggleLayouts Full (layout_grid ||| layout_tall ||| layout_circle ||| layout_tabup ||| layout_mirror_tall ||| layout_magnify_circle ||| layout_magnify_grid ||| layout_trinity_column ||| simpleFloat ||| layout_tall_spread)

-- use xprop to get WM_CLASSNAME
myManageHook = composeAll
      [ className <? [ "Gimp"
                     , "Mplayer"
                     , "Gnome-player"
                     , "Zenity"
                     , "hl2_linux"
                     , "hl_linux"
                     ]       --> doFloat

      , className =? "Conky" --> (ask >>= doF . W.sink)

      , className <? [ "Gcr-prompter"
                     , "Gnome-fallback-mount-helper"
                     ]       --> doCenterFloat

      , className <? [ "Emacs"
                     ]       -->  doF W.shiftMaster

      , className <? [ "Geary"
                     , "Thunderbird"
                     ] --> doF (W.shift "Mail")
      , name =? "Virtual Machine Manager" --> doF W.shiftMaster
      , className =? "xfreerdp" --> doF (W.shift "Rdesktop")
      , name =? "NihongoMod" --> doFloat
      , (role =? "pop-up" <&&> className =? "Google-chrome") -->  doF W.shiftMaster
      , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
      , isFullscreen --> doFullFloat -- for fullscreen apps
      , resource =? "stalonetray" --> doIgnore
      , manageDocks
      ]
      where role = stringProperty "WM_WINDOW_ROLE"
            name = stringProperty "WM_NAME"
            q <? xs = fmap (flip elem xs) q

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. controlMask, xK_Return), spawn $ userHome ++ "/local/bin/small-terminal.sh" ) -- %! Launch terminal xterm
    , ((modMask,               xK_i     ), spawn $ userHome ++ "/local/bin/dmenu.sh") -- %! Launch dmenu
    , ((modMask,               xK_o     ), spawn $ userHome ++ "/local/bin/service.sh") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask              , xK_s     ), spawn $ userHome ++ "/local/bin/screenshot.sh") -- %! screenshot
    , ((modMask .|. shiftMask, xK_s     ), spawn $ userHome ++ "/local/bin/screenshot.sh share") -- %! screenshot
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_l     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Expand) -- %! Expand the master area

    -- move window to other screen
    , ((modMask,               xK_z     ), shiftNextScreen) -- %! Shrink the master area
    , ((modMask .|. shiftMask, xK_z     ), shiftPrevScreen) -- %! Expand the master area


    -- floating layer support
    -- , ((modMask,               xK_f     ), withFocused $ windows . W.float) -- %! Pull window out into floating
    , ((modMask .|. shiftMask, xK_f     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    --, ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]

  ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


gnomeRegisterUtopic :: MonadIO m => m ()
gnomeRegisterUtopic = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.gnome.SessionManager"
            ,"/org/gnome/SessionManager"
            ,"org.gnome.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]

-- vim: ts=2 sw=2 ai et
