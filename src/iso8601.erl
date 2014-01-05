%%%-------------------------------------------------------------------
%%% @author Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%% This application allows to parse and format date, time, intervals
%%% and recurring intervals as described in ISO8601 standard
%%% @end
%%% Created : 2013-07-25 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601).

%% API
-export([
    parse_time/1,
    format_time/2,
    format_time/3,
    parse_localtime/1,
    format_localtime/3,
    format_localtime/5,
    format_timezone/1,
    format_timezone/2,
    parse_date/1,
    format_date/1,
    format_date/2,
    parse_datetime/1,
    format_datetime/3,
    format_datetime/5,
    parse_localdatetime/1,
    format_localdatetime/4,
    format_localdatetime/7,
    apply_timezone_tokens/2
    ]).

%% Internal
-export([
        apply_date_tokens/2,
        apply_time_tokens/3
    ]).

-export_type([
        date_format/0,
        time_format/0,
        timezone_format/0
    ]).

-type date_format() ::
    calendar|
    calendar_extended|
    calendar_month|
    calendar_year|
    calendar_century|
    ordinal|
    ordinal_extended|
    weekday|
    weekday_extended|
    week|
    week_extended .
%%
%% Specifies date format to parse/produce
%%
%% TypeName - Section of the specification - Format - Example
%% <dl>
%% <dt>calendar </dt>
%%	<dd> 4.1.2.2 basic - YYYYMMDD - 20130702</dd>
%% <dt>calendar_extended </dt>
%%	<dd> 4.1.2.2 extended - YYYY-MM-DD - 2013-07-02</dd>
%% <dt>calendar_month </dt>
%%	<dd> 4.1.2.3 a - YYYY-MM - 2013-07</dd>
%% <dt>calendar_year </dt>
%%	<dd> 4.1.2.3 b - YYYY - 2013</dd>
%% <dt>calendar_century </dt>
%%	<dd> 4.1.2.3 c  - YY - 20</dd>
%% <dt>ordinal </dt>
%%	<dd> 4.1.3.2 basic - YYYYDDD - 2013302</dd>
%% <dt>ordinal_extended </dt>
%%	<dd> 4.1.3.2 extended - YYYY-DDD - 2013-302</dd>
%% <dt>weekday </dt>
%%	<dd> 4.1.4.2 Extended - YYYY-Www-D -: 1985-W15-5</dd>
%% <dt>weekday_extended </dt>
%%	<dd> 4.1.4.2 Basic - YYYYWwwD - 1985W155</dd>
%% <dt>week </dt>
%%	<dd> 4.1.4.3 Basic - YYYYWww - 1985W15</dd>
%% <dt>week_extended  </dt>
%%	<dd> 4.1.4.3 Extended - YYYY-Www - 1985-W15</dd>
%% </dl>



-type time_format() ::
    general|
    general_extended|
    general_minute|
    general_minute_extended|
    general_hour|
    general_frac|
    general_extended_frac|
    general_minute_frac|
    general_minute_extended_frac|
    general_hour_frac .
%%
%% Specifies time format to parse/produce
%%
%% TypeName - Section of the specification - Format - Example
%% <dl>
%% <dt>general</dt>
%%	<dd> 4.2.2.2 basic - Thhmmss - 230550</dd>
%% <dt>general_extended</dt>
%%	<dd> 4.2.2.2 extended - Thh:mm:ss - 23:05:50</dd>
%% <dt>general_minute</dt>
%%	<dd> 4.2.2.3.a basic - Thhmm - 2305</dd>
%% <dt>general_minute_extended</dt>
%%	<dd> 4.2.2.3.a extended - Thh:mm - 23:05</dd>
%% <dt>general_hour</dt>
%%	<dd> 4.2.2.3.b basic - Thh - 23</dd>
%% <dt>general_frac</dt>
%%	<dd> 4.2.2.4 basic - Thhmmss,ss* - 230550,12</dd>
%% <dt>general_extended_frac</dt>
%%	<dd> 4.2.2.4 extended - Thh:mm:ss,ss* - 23:05:50,12</dd>
%% <dt>general_minute_frac</dt>
%%	<dd> 4.2.2.4.a basic - Thhmm,mm* - 2305,12</dd>
%% <dt>general_minute_extended_frac</dt>
%%	<dd> 4.2.2.4.a extended - Thh:mm,mm* - 23:05,12</dd>
%% <dt>general_hour_frac</dt>
%%	<dd> 4.2.2.4.b basic - Thh,hh* - 23,12</dd>
%%</dl>
%%

-type timezone_format() ::
    utc|
    hour|
    minute|
    minute_extended.
%%
%% Specifies timezone format
%%
%% <dl>
%% <dt>utc</dt>
%%	<dd> 4.2.4 - UTC  - Z - Z</dd>
%% <dt>hour</dt>
%%	<dd> 4.2.5.1 - hour - [+-]hh - +01</dd>
%% <dt>minute</dt>
%%	<dd> 4.2.5.1 - minute - [+-]hhmm - +0100</dd>
%% <dt>minute</dt>
%%	<dd> 4.2.5.1 - minute_extended - [+-]hh:mm - +01:00</dd>
%%</dl>
%%


-type time_difference() :: {-12..12,0..59 }.

-define( is_num(X), ( is_integer(X) andalso X >= $0 andalso X =< $9) ).
-define( startDate, {0,1,1}).
-define( startTime, {0,0,0}).

%%--------------------------------------------------------------------
%% @doc:	Parses dates autodetecting one of formats specified in standard
%%
%% This function Detects format of the date as specified in ISO 8601 standard section 4.1.
%% The autodetection is based on length, and position of separators ( like "-" )
%% or special characters ( like "W" )
%% @end
%%--------------------------------------------------------------------

-spec(parse_date(Date::binary()|nonempty_string()) -> {Date::calendar:date(),Format::date_format()}).
parse_date(String) when is_list(String) ->
    case parse(String) of
        {date,
         {DetectedFormat, ParsingResult}}  ->
            Date=?MODULE:apply_date_tokens(?startDate,ParsingResult),
            {Date, DetectedFormat};
        _Other ->
            throw({error, {format_mismatch, element(1,_Other)}})
    end.

%%--------------------------------------------------------------------
%% @equiv format_date(Date,calendar_extended)
%% @end
%%--------------------------------------------------------------------

-spec(format_date(calendar:date()) -> nonempty_string() ).

format_date(Date) ->
	?MODULE:format_date(Date,calendar_extended).

%%--------------------------------------------------------------------
%% @doc: Generate string representing date in specific format
%% Function uses one of the many formats to convert date to string
%% @end
%%--------------------------------------------------------------------

-spec(format_date(calendar:date(), Type :: date_format()) -> nonempty_string() ).

format_date({Year, Month, Day}, calendar)
		when is_integer(Year) andalso is_integer(Month)
		andalso is_integer(Day) ->
	lists:flatten(
		io_lib:format("~4..0B~2..0B~2..0B",[Year, Month, Day])
	);
format_date({Year, Month, Day}, calendar_extended)
		when is_integer(Year) andalso is_integer(Month)
		andalso is_integer(Day) ->
	lists:flatten(
		io_lib:format("~4..0B-~2..0B-~2..0B",[Year, Month, Day])
	);
format_date({Year, Month, _Day}, calendar_month)
		when is_integer(Year) andalso is_integer(Month)
		andalso is_integer(_Day) ->
	lists:flatten(
		io_lib:format("~4..0B-~2..0B",[Year, Month])
	);
format_date({Year, _Month, _Day}, calendar_year)
		when is_integer(Year) andalso is_integer(_Month)
		andalso is_integer(_Day) ->
	lists:flatten(
		io_lib:format("~4..0B",[Year])
	);
format_date({Year, _Month, _Day}, calendar_century)
		when is_integer(Year) andalso is_integer(_Month)
		andalso is_integer(_Day) ->
	lists:flatten(
		io_lib:format("~2..0B",[erlang:trunc(Year/100)])
	);
format_date(Date = {Year, Month, Day}, ordinal)
		when is_integer(Year) andalso is_integer(Month)
		andalso is_integer(Day) ->
	Days = calendar:date_to_gregorian_days(Date)
		- calendar:date_to_gregorian_days({Year,1,1}) +1,
	lists:flatten(
		io_lib:format("~4..0B~3..0B",[Year, Days])
	);
format_date(Date = {Year, Month, Day}, ordinal_extended)
		when is_integer(Year) andalso is_integer(Month)
		andalso is_integer(Day) ->
	Days = calendar:date_to_gregorian_days(Date)
		- calendar:date_to_gregorian_days({Year,1,1}) +1,
	lists:flatten(
		io_lib:format("~4..0B-~3..0B",[Year, Days])
	);
format_date(Date = {BaseYear, Month, Day}, weekday)
		when is_integer(BaseYear) andalso is_integer(Month)
		andalso is_integer(Day) ->
	{Year,Week} = calendar:iso_week_number(Date),
	DOW = calendar:day_of_the_week(Date),
	lists:flatten(
		io_lib:format("~4..0BW~2..0B~1..0B",[Year, Week, DOW])
	);
format_date(Date = {BaseYear, Month, Day}, weekday_extended)
		when is_integer(BaseYear) andalso is_integer(Month)
		andalso is_integer(Day) ->
	{Year,Week} = calendar:iso_week_number(Date),
	DOW = calendar:day_of_the_week(Date),
	lists:flatten(
		io_lib:format("~4..0B-W~2..0B-~1..0B",[Year, Week, DOW])
	);
format_date(Date = {BaseYear, Month, Day}, week)
		when is_integer(BaseYear) andalso is_integer(Month)
		andalso is_integer(Day) ->
	{Year,Week} = calendar:iso_week_number(Date),
	lists:flatten(
		io_lib:format("~4..0BW~2..0B",[Year, Week])
	);
format_date(Date = {BaseYear, Month, Day}, week_extended)
		when is_integer(BaseYear) andalso is_integer(Month)
		andalso is_integer(Day) ->
	{Year,Week} = calendar:iso_week_number(Date),
	lists:flatten(
		io_lib:format("~4..0B-W~2..0B",[Year, Week])
	);
format_date({BaseYear, Month, Day}, Type)
		when is_integer(BaseYear) andalso is_integer(Month)
		andalso is_integer(Day) ->
	throw({error,{unknown_format, Type}});
format_date(Junk, _Type) ->
	throw({error,{unknown_data, Junk}}).

%%--------------------------------------------------------------------
%% @doc:	Parses time in specified format
%% @end
%%--------------------------------------------------------------------

-spec(parse_time(String::nonempty_string() )
   -> {
       {Time::calendar:time(),MicroSec :: non_neg_integer()},
       Format::time_format()
   }).

parse_time(String ) when is_list(String) ->
    case parse(String) of
        {time,
         {DetectedFormat, ParsingResult}}  ->
            Time=?MODULE:apply_time_tokens(?startTime,0 ,ParsingResult),
            {Time,DetectedFormat};
        _Other ->
            throw({error, {format_mismatch, element(1,_Other)}})
    end.

%%--------------------------------------------------------------------
%% @doc:    Formats time wigh general, or general_frac format
%% @end
%%--------------------------------------------------------------------

-spec(format_time(Time::calendar:time(), Utime::non_neg_integer() ) -> nonempty_string()).

format_time(Time, Utime ) ->
    if
        Utime>0 -> ?MODULE:format_time(Time, Utime, general_frac);
        true -> ?MODULE:format_time(Time, Utime, general)
    end.

%%--------------------------------------------------------------------
%% @doc:    Formats time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_time(Time::calendar:time(), Utime::non_neg_integer(), Format::time_format() ) -> nonempty_string()).

format_time({H,M,S}, _, general) ->
    lists:flatten(
        io_lib:format("T~2..0B~2..0B~2..0B",[H, M, S])
    );
format_time(_Time, _Utime, Format) ->
    throw({error, {unsupported_format,Format}}).

%%--------------------------------------------------------------------
%% @doc:	Parses localtime (time with timezone)
%% @end
%%--------------------------------------------------------------------

-spec(parse_localtime(String::nonempty_string())
    -> {
        {Time::calendar:time(), MicroSec::non_neg_integer(), TD::time_difference()},
        {TimeFormat::time_format(),TZFormat::timezone_format()}}).

parse_localtime(String) when is_list(String) ->
    case parse(String) of
        {localtime,
            {TimeFormat, ParsingResult},
            {TZFormat, Direction, TZTokens}} ->
                {Time, Usec} = ?MODULE:apply_time_tokens(?startTime,0 ,ParsingResult),
                TZ=?MODULE:apply_timezone_tokens( Direction, TZTokens),
                {{Time, Usec, TZ},{TimeFormat,TZFormat}};
        _Other ->
            throw({error, {format_mismatch, element(1,_Other)}})
    end.

%%--------------------------------------------------------------------
%% @doc:    Formats local time wigh general, or general_frac format
%% @end
%%--------------------------------------------------------------------

-spec(format_localtime(Time::calendar:time(), Utime::non_neg_integer(), TZ::integer() )
      -> nonempty_string()).

format_localtime(Time, Utime, TZ) ->
    ?MODULE:format_time(Time, Utime) ++ ?MODULE:format_timezone(TZ).

%%--------------------------------------------------------------------
%% @doc:    Formats local time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_localtime(Time::calendar:time(), Utime::non_neg_integer(), TZ::integer(),
                       Format::time_format(), TZFormat::timezone_format() )
      -> nonempty_string()).

format_localtime(Time, Utime, TZ, Format, Format_TZ) ->
    ?MODULE:format_time(Time,Utime, Format) ++ ?MODULE:format_timezone(TZ,Format_TZ).

%%--------------------------------------------------------------------
%% @doc:    Formats local time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_timezone(TZ::integer() ) -> nonempty_string()).

format_timezone(TZ)  ->
    ?MODULE:format_timezone(TZ,minute).

%%--------------------------------------------------------------------
%% @doc:    Formats local time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_timezone(TZ::integer(),TZFormat::timezone_format() )
      -> nonempty_string()).

format_timezone(0, utc)  ->
    "Z";
format_timezone(TZ, hour) when is_integer(TZ) ->
    Sign=case TZ<0 of
             true -> $-;
             false -> $+
         end,
    lists:flatten( [Sign |
        io_lib:format("~2..0B",[abs(trunc(TZ div 3600))])
       ]
    );
format_timezone(TZ, minute) when is_integer(TZ) ->
    format_timezone(TZ, hour) ++
    lists:flatten( [ io_lib:format("~2..0B",[abs(trunc(TZ div 60 rem 60))]) ]);
format_timezone(TZ, minute_extended) when is_integer(TZ) ->
    format_timezone(TZ, hour) ++ ":" ++
    lists:flatten( [ io_lib:format("~2..0B",[abs(trunc(TZ div 60 rem 60))]) ]);
format_timezone(TZ, Format) when is_integer(TZ) andalso is_atom(Format) ->
    throw({error, {unknown_format,Format}}).


%%--------------------------------------------------------------------
%% @doc:	Parses datetime (time with date)
%% @end
%%--------------------------------------------------------------------

-spec(parse_datetime(String::nonempty_string())
      -> {
            {Date::calendar:date(), Time::calendar:time(), MicroSec::non_neg_integer(), TD::time_difference()},
            {DateFormat::date_format(),TimeFormat::time_format()}
        }).

parse_datetime(String) when is_list(String)->
    case parse(String) of
        {datetime,
            {DateFormat, DateTokens},
            {TimeFormat, TimeTokens}} ->
                Date=?MODULE:apply_date_tokens(?startDate,DateTokens),
                {Time, Usec} = ?MODULE:apply_time_tokens(?startTime,0 ,TimeTokens),
                {{Date,Time, Usec},{DateFormat,TimeFormat}};
        _Other ->
            throw({error, {format_mismatch, element(1,_Other)}})
    end.

%%--------------------------------------------------------------------
%% @doc:    Formats local time wigh general, or general_frac format
%% @end
%%--------------------------------------------------------------------

-spec(format_datetime(Date::calendar:date(), Time::calendar:time(), Utime::non_neg_integer() ) -> nonempty_string()).

format_datetime(Date, Time, Utime) ->
    if
        Utime>0 -> ?MODULE:format_datetime(Date, Time, Utime, calendar, general_frac);
        true -> ?MODULE:format_datetime(Date, Time, Utime, calendar, general)
    end.

%%--------------------------------------------------------------------
%% @doc:    Formats local time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_datetime(Date::calendar:date(), Time::calendar:time(), Utime::non_neg_integer(),
                      DateFormat::date_format(), TimeFormat::time_format())
      -> nonempty_string()).

format_datetime(Date, Time, Utime, DateFormat, TimeFormat) ->
    ?MODULE:format_date(Date, DateFormat) ++ ?MODULE:format_time(Time,Utime, TimeFormat).

%%--------------------------------------------------------------------
%% @doc:	Parses local datetime (date + time + timezone)
%% @end
%%--------------------------------------------------------------------

-spec(parse_localdatetime(String::nonempty_string())
      -> {
            {Date::calendar:date(), Time::calendar:time(), MicroSec::non_neg_integer(), TD::time_difference()},
            {DateFormat::date_format(),TimeFormat::time_format(),TZFormat::timezone_format()}
        }).

parse_localdatetime(String) when is_list(String) ->
    case parse(String) of
        {datetime_local,
            {DateFormat, DateTokens},
            {TimeFormat, TimeTokens},
            {TZFormat, Direction, TZTokens}
        } ->
            Date=?MODULE:apply_date_tokens(?startDate,DateTokens),
            {Time, Usec} = ?MODULE:apply_time_tokens(?startTime,0 ,TimeTokens),
            TZ=?MODULE:apply_timezone_tokens( Direction, TZTokens),
            {{Date,Time, Usec,TZ},{DateFormat,TimeFormat,TZFormat}};
        _Other ->
            throw({error, {format_mismatch, element(1,_Other)}})
    end.
%%--------------------------------------------------------------------
%% @doc:    Formats local time wigh general, or general_frac format
%% @end
%%--------------------------------------------------------------------

-spec(format_localdatetime(Date::calendar:date(), Time::calendar:time(), Utime::non_neg_integer(), TZ::integer() ) -> nonempty_string()).

format_localdatetime(Date, Time, Utime, TZ) ->
    ?MODULE:format_date(Date) ++ ?MODULE:format_time(Time,Utime) ++ ?MODULE:format_timezone(TZ).

%%--------------------------------------------------------------------
%% @doc:    Formats local time according to supplied format
%% @end
%%--------------------------------------------------------------------

-spec(format_localdatetime(Date::calendar:date(), Time::calendar:time(), Utime::non_neg_integer(), TZ::integer(),
                      DateFormat::date_format(), TimeFormat::time_format(), TZFormat::timezone_format())
      -> nonempty_string()).

format_localdatetime(Date, Time, Utime, TZ, DateFormat, TimeFormat, TZFormat) ->
    ?MODULE:format_date(Date, DateFormat) ++ ?MODULE:format_time(Time,Utime, TimeFormat)
    ++ ?MODULE:format_timezone(TZ, TZFormat).

%%--------------------------------------------------------------------
%% @doc:    Apply tokens from parser to build date
%% @end
%%--------------------------------------------------------------------

apply_date_tokens({_,M,D},[{year,Year}|Elements]) ->
    apply_date_tokens({Year,M,D},Elements);
apply_date_tokens({_,M,D},[{century,Century}|Elements]) ->
    apply_date_tokens({Century*100+1,M,D},Elements);
apply_date_tokens({Y,_,D},[{month,Month}|Elements]) ->
    if
        Month >= 13 ->
            throw ( {error, {month_too_big,Month}});
        Month == 0 ->
            throw ({error, { month_too_low, Month}});
        true -> ok
    end,
    apply_date_tokens({Y,Month,D},Elements);
apply_date_tokens({Y,M,_},[{monthday,Day}|Elements]) ->
    LastDay=calendar:last_day_of_the_month(Y, M),
    if
        Day == 0 ->
            throw ({error,{day_too_low,Day }});
        Day > LastDay ->
            throw ({error,{day_too_big,{Y,M,Day} }});
        true ->
            apply_date_tokens({Y,M,Day},Elements)
    end;
apply_date_tokens({Y,_,_},[{weeknumber,Week},{weekday,Day}|Elements]) ->
    if
        Day =< 0 ->
            throw ({error,{weekday_too_low,Day }});
        Day >= 8 ->
            throw ({error,{weekday_too_big,Day }});
        true ->
            ok
    end,
    check_week_number(Y,Week),
    Date=calendar:gregorian_days_to_date(
        gregorian_days_of_iso_w01_1(Y) +
        (Week-1)*7 + Day - 1
    ),
    apply_date_tokens(Date,Elements);
apply_date_tokens({Y,_,_},[{weeknumber,Week}|Elements]) ->
    check_week_number(Y,Week),
    Date=calendar:gregorian_days_to_date(
        gregorian_days_of_iso_w01_1(Y) +
        (Week-1)*7
    ),
    apply_date_tokens(Date,Elements);
apply_date_tokens({Year,_,_},[{yearday,Day}|Elements]) ->
    case {calendar:is_leap_year(Year) , Day} of
        {true,Day} when Day>366
            -> throw({error, {yearday_too_big, {Year, Day}}});
        {false, Day} when Day>365
            -> throw({error, {yearday_too_big, {Year, Day}}});
        {_, Day} when Day<1
            -> throw({error, {yearday_too_low, {Year, Day}}});
        _Other -> ok
    end,
    Date=
        calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days({Year,1,1})+Day-1),
    apply_date_tokens(Date,Elements);
apply_date_tokens(_,[Element|_]) ->
    throw({unknown_token,Element});
apply_date_tokens(Date,[]) ->
    Date.

%%--------------------------------------------------------------------
%% @doc:    Apply tokens from parser to build time
%% @end
%%--------------------------------------------------------------------
apply_time_tokens({_,M,S},U,[{hour,H}|Elements]) ->
	apply_time_tokens({H,M,S},U,Elements);
apply_time_tokens({H,_,S},U,[{minute,M}|Elements]) ->
	apply_time_tokens({H,M,S},U,Elements);
apply_time_tokens({H,M,_},U,[{second,S}|Elements]) ->
	apply_time_tokens({H,M,S},U,Elements);
apply_time_tokens(OldTime,OldU,[{frac,Base,Numbers}|Elements]) ->
	Seconds=list_to_integer(Numbers),
	SumMicroseconds= OldU + Base *
	if
		length(Numbers)<6 ->
			lists:foldl(
				fun(_,Acc) -> Acc*10 end,
				Seconds,
				lists:seq(1,6-length(Numbers)));
		length(Numbers) == 6 ->
			Seconds;
		length(Numbers) > 6 ->
			lists:foldl(
				fun(_,Acc) -> round(Acc/10) end,
				Seconds,
				lists:seq(1,length(Numbers)-6))
	end,
	Microseconds = SumMicroseconds rem 1000000,
	DiffSeconds = SumMicroseconds div 1000000,

	NewTime=calendar:seconds_to_time(
						calendar:time_to_seconds( OldTime)
						+ DiffSeconds
					 ),
	apply_time_tokens(NewTime,Microseconds,Elements);
apply_time_tokens(_,_,[Element|_]) ->
	throw({unknown_token,Element});
apply_time_tokens(T,U,[]) ->
	{T,U}.


check_week_number(Y,Week) ->
    {_, LastWeekNumber} = calendar:iso_week_number(
        calendar:gregorian_days_to_date(
            gregorian_days_of_iso_w01_1(Y+1)-1
        )
    ),
    if
        Week < 1 ->
            throw ({error,{weeknumber_too_low,Week}});
        Week > LastWeekNumber ->
            throw ({error,{weeknumber_too_big,{Y,Week} }});
        true -> ok
    end.


%%--------------------------------------------------------------------
%% @doc:    Builds timezone difference from tokens
%% @end
%%--------------------------------------------------------------------

-spec(apply_timezone_tokens(Direction::sub|add,Tokens::list()) -> integer()).
apply_timezone_tokens(Direction, Tokens) ->
  TZ=lists:foldl(
    fun({hour,X},Val) -> Val+X*3600;
    ({minute,X},Val) -> Val+X*60 end,
    0,
    Tokens),
  case Direction of
    sub -> TZ*-1;
    add -> TZ
  end.

%%--------------------------------------------------------------------
%% @doc:    Runs lexer and parser to produce tokens from strings
%% @end
%%--------------------------------------------------------------------
parse(String) when is_list(String) ->
  case iso8601_lexer:string( String) of
    {ok,LexTokens,1} ->
      case iso8601_parser:parse(LexTokens) of
        {ok, Tokens}  ->
          Tokens;
        {error, Info } ->
          throw({error, {parsing_failed,Info}})
      end;
    {error, Info, Line} ->
      throw({error, {lexical_analysis_failed,Info, Line}})
  end.

% Helper functions
%
%%
%% The Gregorian days of the iso week 01 day 1 for a given year.
%% copied from calendar module from stdlib
%%
-spec gregorian_days_of_iso_w01_1(non_neg_integer()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
		D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
		DOW = calendar:day_of_the_week(Year, 1, 1),
		if DOW =< 4 ->
	D0101 - DOW + 1;
		true ->
	D0101 + 7 - DOW + 1
		end.

%% vim: set ts=4 sw=4 ai invlist si cul nu:
