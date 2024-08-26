import XMonad
import XMonad.Config
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Actions.NoBorders
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Accordion
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap,mkKeymap)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Font
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86

myTerminal :: String
myTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = True


myBorderWidth = 1

myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor :: String
myFocusedBorderColor = "#FAD32F"

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens :: FocusedOnly -> WindowSet -> Rectangle -> Maybe (W.Stack Window) -> [(Window, Rectangle)] -> [Window]
  hiddens _ wset _ _ wrs =
    case W.peek wset of
      Nothing -> fmap fst wrs
      Just focused -> filter (/= focused) $ fmap fst wrs

myWorkspaces = [" \xf120  ", " \xf0ac  ", " \xee9c  ", " \xeda9  ", " \xef1e  ", " \xf2dc  ", " \xf0b46  ", " \xf11eb  ", " \xefc8  "]

myStartupHook = do
           spawnOnce "picom --fade-in-step=1 --fade-out-step=1 &"
           spawnOnce "udiskie &"
           spawnOnce "dunst &"
           spawnOnce "bash wallpaper.sh"

myKeys = [("M-<Return>", spawn myTerminal),
          ("M-q", kill),
          ("M-S-r", spawn "xmonad --recompile; xmonad --restart"),
          ("M-r", spawn "rofi -show run"),
          ("M-e", spawn "rofi -show emoji"),
          ("M-f", sendMessage (MT.Toggle NBFULL)),
          ("M-g", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled),
          ("M-j", windows $ W.focusDown),
          ("M-k", windows $ W.focusUp),
          ("<XF86MonBrightnessUp>", spawn "lux -a 10%; bash $HOME/.config/xmonad/scripts/brightness_not.sh"),
          ("<XF86MonBrightnessDown>", spawn "lux -s 10%; bash $HOME/.config/xmonad/scripts/brightness_not.sh"),
          ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute; bash $HOME/.config/xmonad/scripts/volume_not.sh"),
          ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute; bash $HOME/.config/xmonad/scripts/volume_not.sh"),
          ("<XF86AudioMute>", spawn "amixer set Master toggle; bash $HOME/.config/xmonad/scripts/volume_not.sh"),
          ("M-S-l", spawn "bash $HOME/.config/xmonad/scripts/i3lock.sh"),
          ("M-w", spawn "bash wallpaper.sh"),
          ("M-S-k", spawn "setxkbmap -query | grep -q 'cz' && setxkbmap us || setxkbmap cz,us")
          -- ("M-S-b", spawn "dbus-send --session --dest=org.Xmobar.Control --type=method_call --print-reply '/org/Xmobar/Control' org.Xmobar.Control.SendSignal \"string:Toggle -1\"" >> (sendMessage $ ToggleStruts) >> refresh)
	  ]

myManageHooks = composeAll 
        [manageDocks,
        className =? "firefox" --> doShift (myWorkspaces !! 1),
        className =? "firefox" --> hasBorder False,
        className =? "usb" --> doShift(myWorkspaces !! 3),
        isFullscreen --> doFullFloat,
        manageHook def]

myFadeHook = composeAll [opaque,
                         isUnfocused <&&> className =? "firefox" --> opacity 0.85
                        ]

fullscrean = NBFULL ?? NOBORDERS ?? EOT
tileWithGaps = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True $ smartBorders $ lessBorders (FocusedOnly) $ ResizableTall 1 (3/100) (1/2) []
tileNoGaps = smartBorders $ ResizableTall 1 (3/100) (1/2) []
accordionNoBorder = noBorders $ Accordion
myLayout = onWorkspace " \xf0ac  " tileNoGaps $ 
           mkToggle (fullscrean) (tileWithGaps ||| accordionNoBorder)


main = do
         xbar <- spawnPipe "xmobar /home/matusu/.config/xmonad/.xmobarrc"
         xmonad $ xmobarProp $ def {
        terminal           = myTerminal,
        startupHook        = myStartupHook,
        manageHook         = myManageHooks,
        layoutHook         = myLayout,
        workspaces         = myWorkspaces,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        handleEventHook = fadeWindowsEventHook,
        logHook = dynamicLogWithPP xmobarPP {
         ppOutput = hPutStrLn xbar,
         ppCurrent = xmobarColor "#FAD32F" "" . wrap "<fc=#0A1124,#FAD32F> " " </fc>",
         ppVisible = xmobarColor "#98be65" "" . wrap " " " ",
         ppHidden = xmobarColor "#EB3247" "" . wrap " " " ",
         ppHiddenNoWindows = xmobarColor "#5F4149" "" . wrap " " " ",
         ppTitle = xmobarColor "#3afc2" "" . shorten 60,
         ppSep = "<fc=#666666> <fn=1>|</fn> </fc>",
         ppUrgent = xmobarColor "#C45500" "",
         ppExtras = [],
         ppOrder = \(ws:_) -> [ws]
         } >> fadeWindowsLogHook myFadeHook
    } `additionalKeysP` myKeys
