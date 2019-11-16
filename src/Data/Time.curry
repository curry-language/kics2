------------------------------------------------------------------------------
--- Library for handling date and time information.
---
--- @author Michael Hanus
--- @version January 2018
--- @category general
------------------------------------------------------------------------------

module Data.Time(ClockTime,
                 CalendarTime(..),ctYear,ctMonth,ctDay,ctHour,ctMin,ctSec,ctTZ,
                 getClockTime,getLocalTime,toUTCTime,toClockTime,toCalendarTime,
                 clockTimeToInt,calendarTimeToString,toDayString,toTimeString,
                 addSeconds,addMinutes,addHours,addDays,addMonths,addYears,
                 daysOfMonth,validDate,compareCalendarTime,compareClockTime
                 ) where


--- ClockTime represents a clock time in some internal representation.
data ClockTime = CTime Int
 deriving (Eq, Ord, Show, Read)

--- A calendar time is presented in the following form:
--- (CalendarTime year month day hour minute second timezone)
--- where timezone is an integer representing the timezone as a difference
--- to UTC time in seconds.
data CalendarTime = CalendarTime Int Int Int Int Int Int Int
 deriving (Eq, Ord, Show, Read)

--- The year of a calendar time.
ctYear :: CalendarTime -> Int
ctYear (CalendarTime y _ _ _ _ _ _) = y

--- The month of a calendar time.
ctMonth :: CalendarTime -> Int
ctMonth (CalendarTime _ m _ _ _ _ _) = m

--- The day of a calendar time.
ctDay :: CalendarTime -> Int
ctDay (CalendarTime _ _ d _ _ _ _) = d

--- The hour of a calendar time.
ctHour :: CalendarTime -> Int
ctHour (CalendarTime _ _ _ h _ _ _) = h

--- The minute of a calendar time.
ctMin :: CalendarTime -> Int
ctMin (CalendarTime _ _ _ _ m _ _) = m

--- The second of a calendar time.
ctSec :: CalendarTime -> Int
ctSec (CalendarTime _ _ _ _ _ s _) = s

--- The time zone of a calendar time. The value of the
--- time zone is the difference to UTC time in seconds.
ctTZ :: CalendarTime -> Int
ctTZ (CalendarTime _ _ _ _ _ _ tz) = tz


--- Returns the current clock time.
getClockTime :: IO ClockTime
getClockTime external

--- Returns the local calendar time.
getLocalTime :: IO CalendarTime
getLocalTime = do
  ctime <- getClockTime
  toCalendarTime ctime

--- Transforms a clock time into a unique integer.
--- It is ensured that clock times that differs in at least one second
--- are mapped into different integers.
clockTimeToInt :: ClockTime -> Int
clockTimeToInt (CTime i) = i

--- Transforms a clock time into a calendar time according to the local time
--- (if possible). Since the result depends on the local environment,
--- it is an I/O operation.
toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime ctime = prim_toCalendarTime $## ctime

prim_toCalendarTime :: ClockTime -> IO CalendarTime
prim_toCalendarTime external

--- Transforms a clock time into a standard UTC calendar time.
--- Thus, this operation is independent on the local time.
toUTCTime :: ClockTime -> CalendarTime
toUTCTime ctime = prim_toUTCTime $## ctime

prim_toUTCTime :: ClockTime -> CalendarTime
prim_toUTCTime external

--- Transforms a calendar time (interpreted as UTC time) into a clock time.
toClockTime :: CalendarTime -> ClockTime
toClockTime d = prim_toClockTime $## d

prim_toClockTime :: CalendarTime -> ClockTime
prim_toClockTime external

--- Transforms a calendar time into a readable form.
calendarTimeToString :: CalendarTime -> String
calendarTimeToString ctime@(CalendarTime y mo d _ _ _ _) =
    shortMonths!!(mo-1) ++ " " ++ show d ++ " " ++
    toTimeString ctime ++ " " ++ show y
  where shortMonths = ["Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"]

--- Transforms a calendar time into a string containing the day, e.g.,
--- "September 23, 2006".
toDayString :: CalendarTime -> String
toDayString (CalendarTime y mo d _ _ _ _) =
    longMonths!!(mo-1) ++ " " ++ show d ++ ", " ++ show y
  where longMonths = ["January","February","March","April","May","June","July",
                      "August","September","October","November","December"]

--- Transforms a calendar time into a string containing the time.
toTimeString :: CalendarTime -> String
toTimeString (CalendarTime _ _ _ h mi s _) =
   digit2 h ++":"++ digit2 mi ++":"++ digit2 s
  where digit2 n = if n<10 then ['0',chr(ord '0' + n)]
                           else show n

--- Adds seconds to a given time.
addSeconds :: Int -> ClockTime -> ClockTime
addSeconds n (CTime ctime) = CTime (ctime + n)

--- Adds minutes to a given time.
addMinutes :: Int -> ClockTime -> ClockTime
addMinutes n (CTime ctime) = CTime (ctime + (n*60))

--- Adds hours to a given time.
addHours :: Int -> ClockTime -> ClockTime
addHours n (CTime ctime) = CTime (ctime + (n*3600))

--- Adds days to a given time.
addDays :: Int -> ClockTime -> ClockTime
addDays n (CTime ctime) = CTime (ctime + (n*86400))

--- Adds months to a given time.
addMonths :: Int -> ClockTime -> ClockTime
addMonths n ctime =
 let CalendarTime y mo d h mi s tz = toUTCTime ctime
     nmo = (mo-1+n) `mod` 12 + 1
 in
 if nmo>0
 then addYears ((mo-1+n) `div` 12)
               (toClockTime (CalendarTime y nmo d h mi s tz))
 else addYears ((mo-1+n) `div` 12 - 1)
               (toClockTime (CalendarTime y (nmo+12) d h mi s tz))

--- Adds years to a given time.
addYears :: Int -> ClockTime -> ClockTime
addYears n ctime = if n==0 then ctime else
  let CalendarTime y mo d h mi s tz = toUTCTime ctime
   in toClockTime (CalendarTime (y+n) mo d h mi s tz)

--- Gets the days of a month in a year.
daysOfMonth :: Int -> Int -> Int
daysOfMonth mo yr =
  if mo/=2
  then [31,28,31,30,31,30,31,31,30,31,30,31] !! (mo-1)
  else if yr `mod` 4 == 0 && (yr `mod` 100 /= 0 || yr `mod` 400 == 0)
       then 29
       else 28

--- Is a date consisting of year/month/day valid?
validDate :: Int -> Int -> Int -> Bool
validDate y m d = m > 0 && m < 13 && d > 0 && d <= daysOfMonth m y

--- Compares two dates (don't use it, just for backward compatibility!).
compareDate :: CalendarTime -> CalendarTime -> Ordering
compareDate = compareCalendarTime

--- Compares two calendar times.
compareCalendarTime :: CalendarTime -> CalendarTime -> Ordering
compareCalendarTime ct1 ct2 =
  compareClockTime (toClockTime ct1) (toClockTime ct2)

--- Compares two clock times.
compareClockTime :: ClockTime -> ClockTime -> Ordering
compareClockTime (CTime time1) (CTime time2)
 | time1<time2 = LT
 | time1>time2 = GT
 | otherwise   = EQ
