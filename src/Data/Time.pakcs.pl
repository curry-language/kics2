%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Data.Time:
%

'Data.Time.getClockTime'('Data.Time.CTime'(CTime)) :- currentClockTime(CTime).

'Data.Time.prim_toCalendarTime'('Data.Time.CTime'(ClockTime),
   'Data.Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,TZ)) :-
	clocktime2localtime(ClockTime,Year,Month,Day,Hour,Min,Sec,TZ).

'Data.Time.prim_toUTCTime'('Data.Time.CTime'(ClockTime),
   'Data.Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,0)) :-
	clocktime2utctime(ClockTime,Year,Month,Day,Hour,Min,Sec).

'Data.Time.prim_toClockTime'('Data.Time.CalendarTime'(Year,Month,Day,Hour,
                                                      Min,Sec,TZ),
                             'Data.Time.CTime'(CTime)) :-
	date2clocktime(Year,Month,Day,Hour,Min,Sec,TZ,CTime).
