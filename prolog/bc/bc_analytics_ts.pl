:- module(bc_analytics_ts, [
    bc_analytics_ts_zero/2, % +Interval, -EmptySeries
    bc_analytics_ts_incr/3,  % +Series, +TimeStamp, -Series
    bc_analytics_ts_list/2   % +Series, -List
]).

/** <module> Generic visitor tracking analytics */

:- use_module(library(assoc)).

% Daily time series prefilled with zeroes.

bc_analytics_ts_zero(Interval, Series):-
    Interval = (YearFrom, MonthFrom)-(YearTo, MonthTo),
    StartDate = date(YearFrom, MonthFrom, 1,
        0, 0, 0, 0, 'UTC', -),
    MonthEnd is MonthTo + 1,
    EndDate = date(YearTo, MonthEnd, 1,
        0, 0, 0, 0, 'UTC', -),
    get_time(CurrentTimeStamp),
    date_time_stamp(EndDate, EndTimeStamp),
    MinEndTimeStamp is min(CurrentTimeStamp, EndTimeStamp),
    prefill_empty(StartDate, MinEndTimeStamp, [], Prefilled),
    list_to_assoc(Prefilled, Series).

% Helper to fill a list with initial values per day.

prefill_empty(CurrentDate, EndTimeStamp, In, Out):-
    date_time_stamp(CurrentDate, CurrentTimeStamp),
    (   CurrentTimeStamp < EndTimeStamp
    ->  stamp_date_time(CurrentTimeStamp, NormalizedDate, 'UTC'),
        format_time(atom(Key), '%F', NormalizedDate),
        add_days(CurrentDate, 1, NextDate),
        prefill_empty(NextDate, EndTimeStamp, [Key-0|In], Out)
    ;   reverse(In, Out)).

% Adds days to the given date.

add_days(Date, Days, NewDate):-
    Date = date(Year, Month, Day,
        Hour, Minute, Second, Offset, Tz, Dst),
    NewDay is Day + Days,
    NewDate = date(Year, Month, NewDay,
        Hour, Minute, Second, Offset, Tz, Dst).

bc_analytics_ts_incr(In, TimeStamp, Out):-
    format_time(atom(Key), '%F', TimeStamp), % UTC?
    (   get_assoc(Key, In, Current)
    ->  New is Current + 1,
        put_assoc(Key, In, New, Out)
    ;   put_assoc(Key, In, 1, Out)).

bc_analytics_ts_list(Series, Dicts):-
    assoc_to_list(Series, List),
    maplist(timeseries_entry_dict, List, Dicts).

timeseries_entry_dict(Date-Count, Dict):-
    Dict = _{ date: Date, count: Count }.
