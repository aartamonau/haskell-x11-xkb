{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.X11.Xkb
       ( OpcodeMajor, OpcodeMinor, EventBase, ErrorBase,
         VersionMajor, VersionMinor,

         EventType,
         newKeyboardNotify, mapNotify, stateNotify, controlsNotify,
         indicatorStateNotify, indicatorMapNotify, namesNotify, compatMapNotify,
         bellNotify, actionMessage, accessXNotify, extensionDeviceNotify,

         Event, EventMask,

         Device,
         useCoreKbd, useCorePtr,
         dfltXIClass, dfltXIId, allXIClasses, allXIIds, xINone,

         StateNotifyEventDetailMask,
         modifierStateMask, modifierBaseMask, modifierLatchMask,
         modifierLockMask, groupStateMask, groupBaseMask, groupLatchMask,
         groupLockMask, compatStateMask, grabModsMask, compatGrabModsMask,
         lookupModsMask, compatLookupModsMask,
         pointerButtonMask, allStateComponentsMask,


         EventDetailMask,

         Group,
         group1Index, group2Index, group3Index, group4Index,

         State,
         StateNotifyEvent,

         versionMajor, versionMinor, versionString,
         libraryVersion, queryExtension, openDisplay, ignoreExtension,
         getEventType, getDevice, getTime, selectEvents, selectEventDetails,
         lockGroup, latchGroup, lockModifiers, latchModifiers,
         getState, getStateNotifyEvent
       )
       where

import Foreign
import Foreign.C

import Control.Exception ( assert )

import qualified Graphics.X11 as X11

#include <X11/XKBlib.h>

type OpcodeMajor  = #{type int}
type OpcodeMinor  = #{type int}
type EventBase    = #{type int}
type ErrorBase    = #{type int}
type VersionMajor = #{type int}
type VersionMinor = #{type int}

versionMajor :: VersionMajor
versionMajor = #{const XkbMajorVersion}

versionMinor :: VersionMinor
versionMinor = #{const XkbMinorVersion}

versionString :: String
versionString = show versionMajor ++ "." ++ show versionMinor

foreign import ccall unsafe "XkbLibraryVersion"
  xkbLibraryVersion :: Ptr VersionMajor -> Ptr VersionMinor -> IO Bool

libraryVersion :: IO (Bool, VersionMajor, VersionMinor)
libraryVersion =
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor -> do
    poke p_versionMajor versionMajor
    poke p_versionMinor versionMinor

    compatible <- xkbLibraryVersion p_versionMajor p_versionMinor

    libVersionMajor <- peek p_versionMajor
    libVersionMinor <- peek p_versionMinor

    return (compatible, libVersionMajor, libVersionMinor)

foreign import ccall unsafe "XkbQueryExtension"
  xkbQueryExtension :: X11.Display
                    -> Ptr OpcodeMajor
                    -> Ptr EventBase
                    -> Ptr ErrorBase
                    -> Ptr VersionMajor
                    -> Ptr VersionMinor
                    -> IO Bool

queryExtension :: X11.Display
               -> IO (Either (VersionMajor, VersionMinor)
                             (OpcodeMajor, EventBase, ErrorBase))
queryExtension dpy =
  alloca $ \p_opcodeMajor ->
  alloca $ \p_eventBase ->
  alloca $ \p_errorBase ->
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor -> do
    poke p_versionMajor versionMajor
    poke p_versionMinor versionMinor

    ret <- xkbQueryExtension dpy p_opcodeMajor p_eventBase p_errorBase
                             p_versionMajor p_versionMinor

    if ret
      then do
        opcodeMajor <- peek p_opcodeMajor
        eventBase   <- peek p_eventBase
        errorBase   <- peek p_errorBase

        return $ Right (opcodeMajor, eventBase, errorBase)
      else do
        serverMajor <- peek p_versionMajor
        serverMinor <- peek p_versionMinor

        return $ Left (serverMajor, serverMinor)

type OpenDisplayStatus = #{type int}
#{enum OpenDisplayStatus,
 , xkbOD_Success           = XkbOD_Success
 , xkbOD_BadLibraryVersion = XkbOD_BadLibraryVersion
 , xkbOD_ConnectionRefused = XkbOD_ConnectionRefused
 , xkbOD_NonXkbServer      = XkbOD_NonXkbServer
 , xkbOD_BadServerVersion  = XkbOD_BadServerVersion
 }

foreign import ccall unsafe "XkbOpenDisplay"
  xkbOpenDisplay :: CString
                 -> Ptr EventBase
                 -> Ptr ErrorBase
                 -> Ptr VersionMajor
                 -> Ptr VersionMinor
                 -> Ptr OpenDisplayStatus
                 -> IO (Ptr X11.Display)

openDisplay :: String -> IO (EventBase, ErrorBase, X11.Display)
openDisplay name =
  withCString name $ \c_name ->
  alloca $ \p_eventBase ->
  alloca $ \p_errorBase ->
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor ->
  alloca $ \p_status -> do
      poke p_versionMajor versionMajor
      poke p_versionMinor versionMinor

      dpy <- xkbOpenDisplay c_name p_eventBase p_errorBase
                            p_versionMajor p_versionMinor p_status

      status <- peek p_status
      maybeThrow dpy status

      eventBase <- peek p_eventBase
      errorBase <- peek p_errorBase

      return (eventBase, errorBase, X11.Display dpy)

  where maybeThrow :: Ptr X11.Display -> OpenDisplayStatus -> IO ()
        maybeThrow dpy status
          | status == xkbOD_Success = assert (dpy /= nullPtr) $ return ()
          | otherwise =
              assert (dpy == nullPtr) $ throw (errorMsg status)

        throw :: String -> IO ()
        throw = ioError . userError

        errorMsg :: OpenDisplayStatus -> String
        errorMsg status
          | status == xkbOD_BadLibraryVersion =
            "xlib of incompatible version with ours: " ++ versionString
          | status == xkbOD_BadServerVersion =
            "xserver of incompatible version with ours: " ++ versionString
          | status == xkbOD_ConnectionRefused = "failed to open display"
          | status == xkbOD_NonXkbServer = "XKB extension not present"
          | otherwise =
            error "Impossible happened. Got display status " ++ show status

foreign import ccall unsafe "XkbIgnoreExtension"
  ignoreExtension :: Bool -> IO Bool

type EventType = #{type int}
#{enum EventType,
  , newKeyboardNotify     = XkbNewKeyboardNotify
  , mapNotify             = XkbMapNotify
  , stateNotify           = XkbStateNotify
  , controlsNotify        = XkbControlsNotify
  , indicatorStateNotify  = XkbIndicatorStateNotify
  , indicatorMapNotify    = XkbIndicatorMapNotify
  , namesNotify           = XkbNamesNotify
  , compatMapNotify       = XkbCompatMapNotify
  , bellNotify            = XkbBellNotify
  , actionMessage         = XkbActionMessage
  , accessXNotify         = XkbAccessXNotify
  , extensionDeviceNotify = XkbExtensionDeviceNotify
  }

newtype Event = Event { eventPtr :: EventPtr }
type EventPtr = Ptr Event
type EventMask = #{type unsigned long int}

type Device = #{type unsigned int}
#{enum Device,
 , useCoreKbd   = XkbUseCoreKbd
 , useCorePtr   = XkbUseCorePtr
 , dfltXIClass  = XkbDfltXIClass
 , dfltXIId     = XkbDfltXIId
 , allXIClasses = XkbAllXIClasses
 , allXIIds     = XkbAllXIIds
 , xINone       = XkbXINone
 }

getEventType :: Event -> IO EventType
getEventType = #{peek XkbAnyEvent, xkb_type} . eventPtr

getDevice :: Event -> IO Device
getDevice = #{peek XkbAnyEvent, device} . eventPtr

getTime :: Event -> IO X11.Time
getTime = #{peek XkbAnyEvent, time} . eventPtr


foreign import ccall unsafe "XkbSelectEvents"
  selectEvents :: X11.Display
               -> Device
               -> EventMask
               -> EventMask
               -> IO Bool

type StateNotifyEventDetailMask = #{type int}
#{enum StateNotifyEventDetailMask,
  , modifierStateMask      = XkbModifierStateMask
  , modifierBaseMask       = XkbModifierBaseMask
  , modifierLatchMask      = XkbModifierLatchMask
  , modifierLockMask       = XkbModifierLockMask
  , groupStateMask         = XkbGroupStateMask
  , groupBaseMask          = XkbGroupBaseMask
  , groupLatchMask         = XkbGroupLatchMask
  , groupLockMask          = XkbGroupLockMask
  , compatStateMask        = XkbCompatStateMask
  , grabModsMask           = XkbGrabModsMask
  , compatGrabModsMask     = XkbCompatGrabModsMask
  , lookupModsMask         = XkbLookupModsMask
  , compatLookupModsMask   = XkbCompatLookupModsMask
  , pointerButtonMask      = XkbPointerButtonMask
  , allStateComponentsMask = XkbAllStateComponentsMask
  }

type EventDetailMask = #{type unsigned long int}

foreign import ccall unsafe "XkbSelectEventDetails"
  selectEventDetails :: X11.Display
                     -> Device
                     -> EventType
                     -> EventDetailMask
                     -> EventDetailMask
                     -> IO Bool

type Group = #{type int}
#{enum Group,
 , group1Index = XkbGroup1Index
 , group2Index = XkbGroup2Index
 , group3Index = XkbGroup3Index
 , group4Index = XkbGroup4Index
 }

foreign import ccall unsafe "XkbLockGroup"
  lockGroup :: X11.Display
            -> Device
            -> Group
            -> IO Bool

foreign import ccall unsafe "XkbLatchGroup"
  latchGroup :: X11.Display
             -> Device
             -> Group
             -> IO Bool

foreign import ccall unsafe "XkbLockModifiers"
  lockModifiers :: X11.Display
                -> Device
                -> X11.KeyMask
                -> X11.KeyMask
                -> IO Bool

foreign import ccall unsafe "XkbLatchModifiers"
  latchModifiers :: X11.Display
                 -> Device
                 -> X11.KeyMask
                 -> X11.KeyMask
                 -> IO Bool

type State =
  ( Group                      -- group
  , Group                      -- base_group
  , Group                      -- latched_group
  , Group                      -- locked_group
  , X11.KeyMask                   -- mods
  , X11.KeyMask                   -- base_mods
  , X11.KeyMask                   -- latched_mods
  , X11.KeyMask                   -- locked_mods
  , X11.KeyMask                   -- compat_state
  , X11.KeyMask                   -- grab_mods
  , X11.KeyMask                   -- compat_grab_mods
  , X11.KeyMask                   -- lookup_mods
  , X11.KeyMask                   -- compat_lookup_mods
  , X11.ButtonMask                -- ptr_buttons
  )

allocaState :: (Ptr State -> IO a) -> IO a
allocaState = allocaBytes #{size XkbStateRec}

peekState :: Ptr State -> IO State
peekState p = do
  group              <- #{peek XkbStateRec, group} p
  base_group         <- #{peek XkbStateRec, base_group} p
  latched_group      <- #{peek XkbStateRec, latched_group} p
  locked_group       <- #{peek XkbStateRec, locked_group} p
  mods               <- #{peek XkbStateRec, mods} p
  base_mods          <- #{peek XkbStateRec, base_mods} p
  latched_mods       <- #{peek XkbStateRec, latched_mods} p
  locked_mods        <- #{peek XkbStateRec, locked_mods} p
  compat_state       <- #{peek XkbStateRec, compat_state} p
  grab_mods          <- #{peek XkbStateRec, grab_mods} p
  compat_grab_mods   <- #{peek XkbStateRec, compat_grab_mods} p
  lookup_mods        <- #{peek XkbStateRec, lookup_mods} p
  compat_lookup_mods <- #{peek XkbStateRec, compat_lookup_mods} p
  ptr_buttons        <- #{peek XkbStateRec, ptr_buttons} p

  return (group,
          base_group,
          latched_group,
          locked_group,
          mods,
          base_mods,
          latched_mods,
          locked_mods,
          compat_state,
          grab_mods,
          compat_grab_mods,
          lookup_mods,
          compat_lookup_mods,
          ptr_buttons)

foreign import ccall unsafe "XkbGetState"
  xkbGetState :: X11.Display
              -> Device
              -> Ptr State
              -> IO X11.Status

getState :: X11.Display -> Device -> IO (Maybe State)
getState dpy device =
  allocaState $ \p_state -> do
    status <- xkbGetState dpy device p_state
    if status == 0
      then fmap Just (peekState p_state)
      else return Nothing

type StateNotifyEvent =
  ( StateNotifyEventDetailMask -- changed
  , Group                      -- group
  , Group                      -- base_group
  , Group                      -- latched_group
  , Group                      -- locked_group
  , X11.KeyMask                   -- mods
  , X11.KeyMask                   -- base_mods
  , X11.KeyMask                   -- latched_mods
  , X11.KeyMask                   -- locked_mods
  , X11.KeyMask                   -- compat_state
  , X11.KeyMask                   -- grab_mods
  , X11.KeyMask                   -- compat_grab_mods
  , X11.KeyMask                   -- lookup_mods
  , X11.KeyMask                   -- compat_lookup_mods
  , X11.ButtonMask                -- ptr_buttons
  , X11.KeyCode                   -- keycode
  , X11.EventType                 -- event_type
  , OpcodeMajor                   -- req_major
  , OpcodeMinor                   -- req_minor
  )

allocaStateNotifyEvent :: (Ptr StateNotifyEvent -> IO a) -> IO a
allocaStateNotifyEvent = allocaBytes #{size XkbStateNotifyEvent}

peekStateNotifyEvent :: Ptr StateNotifyEvent -> IO StateNotifyEvent
peekStateNotifyEvent p = do
  changed            <- #{peek XkbStateNotifyEvent, changed} p
  group              <- #{peek XkbStateNotifyEvent, group} p
  base_group         <- #{peek XkbStateNotifyEvent, base_group} p
  latched_group      <- #{peek XkbStateNotifyEvent, latched_group} p
  locked_group       <- #{peek XkbStateNotifyEvent, locked_group} p
  mods               <- #{peek XkbStateNotifyEvent, mods} p
  base_mods          <- #{peek XkbStateNotifyEvent, base_mods} p
  latched_mods       <- #{peek XkbStateNotifyEvent, latched_mods} p
  locked_mods        <- #{peek XkbStateNotifyEvent, locked_mods} p
  compat_state       <- #{peek XkbStateNotifyEvent, compat_state} p
  grab_mods          <- #{peek XkbStateNotifyEvent, grab_mods} p
  compat_grab_mods   <- #{peek XkbStateNotifyEvent, compat_grab_mods} p
  lookup_mods        <- #{peek XkbStateNotifyEvent, lookup_mods} p
  compat_lookup_mods <- #{peek XkbStateNotifyEvent, compat_lookup_mods} p
  ptr_buttons        <- #{peek XkbStateNotifyEvent, ptr_buttons} p
  keycode            <- #{peek XkbStateNotifyEvent, keycode} p
  event_type         <- #{peek XkbStateNotifyEvent, event_type} p
  req_major          <- #{peek XkbStateNotifyEvent, req_major} p
  req_minor          <- #{peek XkbStateNotifyEvent, req_minor} p

  return (changed,
          group,
          base_group,
          latched_group,
          locked_group,
          mods,
          base_mods,
          latched_mods,
          locked_mods,
          compat_state,
          grab_mods,
          compat_grab_mods,
          lookup_mods,
          compat_lookup_mods,
          ptr_buttons,
          keycode,
          event_type,
          req_major,
          req_minor)

getStateNotifyEvent :: Event -> IO StateNotifyEvent
getStateNotifyEvent = peekStateNotifyEvent . castPtr . eventPtr
