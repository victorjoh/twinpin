module EventUtil where

import SDL

noKeyModifier :: KeyModifier
noKeyModifier = KeyModifier False
                            False
                            False
                            False
                            False
                            False
                            False
                            False
                            False
                            False
                            False

enterPressed :: Event
enterPressed =
  toEvent $
    KeyboardEventData
      Nothing
      Pressed
      False
      (Keysym (Scancode 40) (Keycode 13) noKeyModifier)

class EventContent c where
  toEventPayload :: c -> EventPayload
  toEvent :: c -> Event
  toEvent = Event 0 . toEventPayload
  toEvents :: [c] -> [Event]
  toEvents = map toEvent

instance EventContent JoyAxisEventData where
  toEventPayload = JoyAxisEvent

instance EventContent JoyButtonEventData where
  toEventPayload = JoyButtonEvent

instance EventContent JoyHatEventData where
  toEventPayload = JoyHatEvent

instance EventContent KeyboardEventData where
  toEventPayload = KeyboardEvent

instance EventContent JoyDeviceEventData where
  toEventPayload = JoyDeviceEvent