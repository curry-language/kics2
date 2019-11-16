{-# LANGUAGE MultiParamTypeClasses #-}
import qualified System.Time        as T
import qualified Data.Time.Clock    as Clock
import qualified Data.Time.Calendar as Cal

instance ConvertCurryHaskell C_ClockTime T.ClockTime where
  fromCurry (C_CTime i) = T.TOD (fromCurry i) 0
  toCurry   (T.TOD i _) = C_CTime (toCurry i)

instance ConvertCurryHaskell C_CalendarTime T.CalendarTime where
  fromCurry (C_CalendarTime y m d h min s tz) =
    T.CalendarTime  (fromCurry y)
                    (toEnum (fromCurry m - 1))
                    (fromCurry d)
                    (fromCurry h)
                    (fromCurry min)
                    (fromCurry s)
                    0 undefined undefined undefined
                    (fromCurry tz)
                    undefined

  toCurry (T.CalendarTime y m d h min s _ _ _ _ tz _) =
    C_CalendarTime  (toCurry y)
                    (toCurry (fromEnum m + 1))
                    (toCurry d)
                    (toCurry h)
                    (toCurry min)
                    (toCurry s)
                    (toCurry tz)

instance ConvertCurryHaskell C_ClockTime Clock.UTCTime where
  fromCurry ct = let (T.CalendarTime y m d h min s _ _ _ _ tz _)
                        = T.toUTCTime (fromCurry ct)
                 in  fromIntegral tz
                     `Clock.addUTCTime`
                     Clock.UTCTime (Cal.fromGregorian (toInteger y) (fromEnum m + 1) d)
                                  (Clock.secondsToDiffTime (toInteger ((h * 60 + min) * 60 + s)))

  toCurry (Clock.UTCTime day diff) = 
   let (y,m,d) = Cal.toGregorian day in
      toCurry (T.addToClockTime  
                  (T.TimeDiff 0 0 0 0 0 (round (toRational diff)) 0)
                  (T.toClockTime (T.CalendarTime (fromIntegral y)
                                                 (toEnum (m - 1))
                                                 d 0 0 0 0 undefined
                                                 undefined undefined 0 undefined)))

external_d_C_getClockTime :: Cover -> ConstStore -> Curry_Prelude.C_IO C_ClockTime
external_d_C_getClockTime _ _ = toCurry T.getClockTime

external_d_C_prim_toCalendarTime :: C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CalendarTime
external_d_C_prim_toCalendarTime ct _ _ = toCurry T.toCalendarTime ct

external_d_C_prim_toUTCTime :: C_ClockTime -> Cover -> ConstStore -> C_CalendarTime
external_d_C_prim_toUTCTime ct _ _ = toCurry T.toUTCTime ct

external_d_C_prim_toClockTime :: C_CalendarTime -> Cover -> ConstStore -> C_ClockTime
external_d_C_prim_toClockTime ct _ _ = toCurry T.toClockTime ct
