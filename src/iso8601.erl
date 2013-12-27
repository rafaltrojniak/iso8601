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
		parse_date/1,
		parse_date/2,
		format_date/1,
		format_date/2,
		parse_time/1,
		parse_localtime/2
	]).

-export_type([
		date_format/0,
		time_format/0,
		localtime_format/0
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
	general_hour_frac|
	designed_general|
	designed_general_extended|
	designed_general_minute|
	designed_general_minute_extended|
	designed_general_hour|
	designed_general_frac|
	designed_general_extended_frac|
	designed_general_minute_frac|
	designed_general_minute_extended_frac|
	designed_general_hour_frac
	.
%%
%% Specifies time format to parse/produce
%%
%% TypeName - Section of the specification - Format - Example
%% <dl>
%% <dt>general</dt>
%%	<dd> 4.2.2.2 basic - hhmmss - 230550</dd>
%% <dt>general_extended</dt>
%%	<dd> 4.2.2.2 extended - hh:mm:ss - 23:05:50</dd>
%% <dt>general_minute</dt>
%%	<dd> 4.2.2.3.a basic - hhmm - 2305</dd>
%% <dt>general_minute_extended</dt>
%%	<dd> 4.2.2.3.a extended - hh:mm - 23:05</dd>
%% <dt>general_hour</dt>
%%	<dd> 4.2.2.3.b basic - hh - 23</dd>
%% <dt>general_frac</dt>
%%	<dd> 4.2.2.4 basic - hhmmss,ss* - 230550,12</dd>
%% <dt>general_extended_frac</dt>
%%	<dd> 4.2.2.4 extended - hh:mm:ss,ss* - 23:05:50,12</dd>
%% <dt>general_minute_frac</dt>
%%	<dd> 4.2.2.4.a basic - hhmm,mm* - 2305,12</dd>
%% <dt>general_minute_extended_frac</dt>
%%	<dd> 4.2.2.4.a extended - hh:mm,mm* - 23:05,12</dd>
%% <dt>general_hour_frac</dt>
%%	<dd> 4.2.2.4.b basic - hh,hh* - 23,12</dd>
%% <dt>designed_general</dt>
%%	<dd> 4.2.2.2 basic - Thhmmss - T230550</dd>
%% <dt>designed_general_extended</dt>
%%	<dd> 4.2.2.2 extended - Thh:mm:ss - T23:05:50</dd>
%% <dt>designed_general_minute</dt>
%%	<dd> 4.2.2.3.a basic - Thhmm - T2305</dd>
%% <dt>designed_general_minute_extended</dt>
%%	<dd> 4.2.2.3.a extended - Thh:mm - T23:05</dd>
%% <dt>designed_general_hour</dt>
%%	<dd> 4.2.2.3.b basic - Thh - T23</dd>
%% <dt>designed_general_frac</dt>
%%	<dd> 4.2.2.4 basic - Thhmmss,ss* - T230550,12</dd>
%% <dt>designed_general_extended_frac</dt>
%%	<dd> 4.2.2.4 extended - Thh:mm:ss,ss* - T23:05:50,12</dd>
%% <dt>designed_general_minute_frac</dt>
%%	<dd> 4.2.2.4.a basic - Thhmm,mm* - T2305,12</dd>
%% <dt>designed_general_minute_extended_frac</dt>
%%	<dd> 4.2.2.4.a extended - Thh:mm,mm* - T23:05,12</dd>
%% <dt>designed_general_hour_frac</dt>
%%	<dd> 4.2.2.4.b basic - Thh,hh* - T23,12</dd>
%% </dl>
%%

-type localtime_format() ::
	general_tz|
	general_extended_tz|
	general_minute_tz|
	general_minute_extended_tz|
	general_hour_tz|
	general_frac_tz|
	general_extended_frac_tz|
	general_minute_frac_tz|
	general_minute_extended_frac_tz|
	general_hour_frac_tz|
	designed_general_tz|
	designed_general_extended_tz|
	designed_general_minute_tz|
	designed_general_minute_extended_tz|
	designed_general_hour_tz|
	designed_general_frac_tz|
	designed_general_extended_frac_tz|
	designed_general_minute_frac_tz|
	designed_general_minute_extended_frac_tz|
	designed_general_hour_frac_tz
	.

%%
%% Specifies time format to parse/produce including timezone
%%
%% TypeName - Section of the specification - Format - Example
%% <dl>
%% <dt>general_tz</dt>
%%	<dd> 4.2.2.2 basic - hhmmss+(Z|dd:dd) - 230550+01:00</dd>
%% <dt>general_extended_tz</dt>
%%	<dd> 4.2.2.2 extended - hh:mm:ss+(Z|dd:dd) - 23:05:50+01:00</dd>
%% <dt>general_minute_tz</dt>
%%	<dd> 4.2.2.3.a basic - hhmm+(Z|dd:dd) - 2305+01:00</dd>
%% <dt>general_minute_extended_tz</dt>
%%	<dd> 4.2.2.3.a extended - hh:mm+(Z|dd:dd) - 23:05+01:00</dd>
%% <dt>general_hour_tz</dt>
%%	<dd> 4.2.2.3.b basic - hh+(Z|dd:dd) - 23+01:00</dd>
%% <dt>general_frac_tz</dt>
%%	<dd> 4.2.2.4 basic - hhmmss,ss*+(Z|dd:dd) - 230550,12+01:00</dd>
%% <dt>general_extended_frac_tz</dt>
%%	<dd> 4.2.2.4 extended - hh:mm:ss,ss*+(Z|dd:dd) - 23:05:50,12+01:00</dd>
%% <dt>general_minute_frac_tz</dt>
%%	<dd> 4.2.2.4.a basic - hhmm,mm*+(Z|dd:dd) - 2305,12+01:00</dd>
%% <dt>general_minute_extended_frac_tz</dt>
%%	<dd> 4.2.2.4.a extended - hh:mm,mm*+(Z|dd:dd) - 23:05,12+01:00</dd>
%% <dt>general_hour_frac_tz</dt>
%%	<dd> 4.2.2.4.b basic - hh,hh*+(Z|dd:dd) - 23,12+01:00</dd>
%% <dt>designed_general_tz</dt>
%%	<dd> 4.2.2.2 basic - Thhmmss+(Z|dd:dd) - T230550+01:00</dd>
%% <dt>designed_general_extended_tz</dt>
%%	<dd> 4.2.2.2 extended - Thh:mm:ss+(Z|dd:dd) - T23:05:50+01:00</dd>
%% <dt>designed_general_minute_tz</dt>
%%	<dd> 4.2.2.3.a basic - Thhmm+(Z|dd:dd) - T2305+01:00</dd>
%% <dt>designed_general_minute_extended_tz</dt>
%%	<dd> 4.2.2.3.a extended - Thh:mm+(Z|dd:dd) - T23:05+01:00</dd>
%% <dt>designed_general_hour_tz</dt>
%%	<dd> 4.2.2.3.b basic - Thh+(Z|dd:dd) - T23+01:00</dd>
%% <dt>designed_general_frac_tz</dt>
%%	<dd> 4.2.2.4 basic - Thhmmss,ss*+(Z|dd:dd) - T230550,12+01:00</dd>
%% <dt>designed_general_extended_frac_tz</dt>
%%	<dd> 4.2.2.4 extended - Thh:mm:ss,ss*+(Z|dd:dd) - T23:05:50,12+01:00</dd>
%% <dt>designed_general_minute_frac_tz</dt>
%%	<dd> 4.2.2.4.a basic - Thhmm,mm*+(Z|dd:dd) - T2305,12+01:00</dd>
%% <dt>designed_general_minute_extended_frac_tz</dt>
%%	<dd> 4.2.2.4.a extended - Thh:mm,mm*+(Z|dd:dd) - T23:05,12+01:00</dd>
%% <dt>designed_general_hour_frac_tz</dt>
%%	<dd> 4.2.2.4.b basic - Thh,hh*+(Z|dd:dd) - T23,12+01:00</dd>
%% </dl>
%%

%% Time parser
%% DEC  = 0-9
%% Hour = 00
%% Hour = 1, DEC
%% Hour = 2,0-3
%% Minute = M1,DEC
%% M1 = 0-5
%% Second = S1, Dec
%% Second = 60
%% S1 = 0-5
%% Frac = DEC+
%% general = Hour, Minute, Second
%% general_extended = Hour, ':', Minute, ':', Second
%% general_minute = Hour, Minute
%% general_minute_extended = Hour, ':', Minute
%% general_hour = Hour
%% general_frac = Hour, Minute, Second, ',', Frac
%% general_extended_frac = Hour, ':', Minute ':',, Second, ',', Frac
%% general_minute_frac = Hour, Minute,  ',', Frac
%% general_minute_extended_frac = Hour, ':', Minute,  ',', Frac
%% general_hour_frac = Hour, ',', Frac
%%
%% Formats= general| general_extended| general_minute| general_minute_extended| general_hour| general_frac| general_extended_frac| general_minute_frac| general_minute_extended_frac| general_hour_frac
%% Designed = 'T',Formats
%% Start = Deisgned | Formats
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

-spec(parse_date(Date::binary()|nonempty_string()) -> Date::calendar:date()).
parse_date(String) when is_binary(String) ->
	parse_date(binary_to_list(String));
parse_date(String) ->
    {ok,LexTokens,1} = iso8601_lexer:string( String),
    case iso8601_parser:parse(LexTokens) of
        {ok, {date, {_DetectedFormat, ParsingResult} }}  ->
            apply_date_tokens(?startDate,ParsingResult);
        {error,Info} ->
            throw({error, {failed_to_parse,Info}})
    end.



%%--------------------------------------------------------------------
%% @doc:	Parses dates in one of formats specified in standard
%%
%% This function checks if string is properly formated according to specified format.
%% @end
%%--------------------------------------------------------------------

-spec(parse_date(Date::binary()|nonempty_string(), Format::date_format()) -> Date::calendar:date()).

parse_date(Date, Format)
		when is_binary(Date) andalso is_atom(Format) ->
	parse_date(binary_to_list(Date), Format);
% Section 4.1.2.3 c - century
parse_date(String, Format) ->
    {ok,LexTokens,1} = iso8601_lexer:string( String),
    case iso8601_parser:parse(LexTokens) of
        {ok, {date, {DetectedFormat, ParsingResult} }}  ->
            if
                DetectedFormat =:= Format ->
                    apply_date_tokens(?startDate,ParsingResult);
                true ->
                    throw({error, {wrong_format,DetectedFormat}})
            end;
        {error,Info} ->
            throw({error, {failed_to_parse,Info}})
    end.



%%--------------------------------------------------------------------
%% @equiv format_date(Date,calendar_extended)
%% @end
%%--------------------------------------------------------------------

-spec(format_date(calendar:date()) -> nonempty_string() ).

format_date(Date) ->
	format_date(Date,calendar_extended).

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

-spec(parse_time(Value::nonempty_string() )
			-> {Time::calendar:time(),MicroSec :: non_neg_integer()}).

parse_time("T" ++ Value ) ->
	{ok,LexTokens,1} = iso8601_lexer:string("T" ++ Value),
	{ok,ParsingResult } = iso8601_parser:parse(LexTokens),
	{_Date,Time,Micro}= build_moment(ParsingResult),
	{Time,Micro};
parse_time( Value ) ->
	parse_time( "T" ++ Value ).

build_moment({time, {_Format,Tokens}}) ->
		apply_tokens(?startDate,?startTime,0,Tokens);
build_moment({date, {_Format,Tokens}}) ->
		apply_tokens(?startDate,?startTime,0,Tokens);
build_moment({localtime, {_TimeFormat,TimeTokens}, {_TZFormat, TZTokens}}) ->
		{Date,Time,Micro}=apply_tokens(?startDate,?startTime,0,TimeTokens),
		apply_tokens(Date,Time,Micro,TZTokens);
build_moment({datetime, {_DateFormat,DateTokens}, {_TimeFormat, TimeTokens}}) ->
		{Date,Date,Micro}=apply_tokens(?startDate,?startTime,0,DateTokens),
		apply_tokens(Date,Date,Micro,TimeTokens);
build_moment({datetime_local, {_DateFormat,DateTokens}, {_TimeFormat, TimeTokens},{_TZFormat, TZTokens}}) ->
		{Date,Date,Micro}=apply_tokens(?startDate,?startTime,0,DateTokens),
		{Date2,Date2,Micro2}=apply_tokens(Date,Date,Micro,TimeTokens),
		apply_tokens(Date2,Date2,Micro2,TZTokens);
build_moment(Unknown) ->
	throw({error, {wrong_format,Unknown}}).

apply_tokens(Date,{_,M,S},U,[{hour,H}|Elements]) ->
	apply_tokens(Date,{H,M,S},U,Elements);
apply_tokens(Date,{H,_,S},U,[{minute,M}|Elements]) ->
	apply_tokens(Date,{H,M,S},U,Elements);
apply_tokens(Date,{H,M,_},U,[{second,S}|Elements]) ->
	apply_tokens(Date,{H,M,S},U,Elements);
apply_tokens(Date,OldTime,OldU,[{frac,Base,Numbers}|Elements]) ->
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
	apply_tokens(Date,NewTime,Microseconds,Elements);
apply_tokens({_,M,D},Time,Utime,[{year,Year}|Elements]) ->
	apply_tokens({Year,M,D},Time,Utime,Elements);
apply_tokens({_,M,D},Time,Utime,[{century,Century}|Elements]) ->
	apply_tokens({Century*100+1,M,D},Time,Utime,Elements);
apply_tokens({Y,_,D},Time,Utime,[{month,Month}|Elements]) ->
	if
		Month >= 13 ->
			throw ( {error, {month_too_big,Month}});
		Month == 0 ->
				throw ({error, { month_too_low, Month}});
		true -> ok
	end,
	apply_tokens({Y,Month,D},Time,Utime,Elements);
apply_tokens({Y,M,_},Time,Utime,[{monthday,Day}|Elements]) ->
	LastDay=calendar:last_day_of_the_month(Y, M),
	if
		Day == 0 ->
			throw ({error,{day_too_low,Day }});
		Day > LastDay ->
			throw ({error,{day_too_big,{Y,M,Day} }});
		true ->
			apply_tokens({Y,M,Day},Time,Utime,Elements)
	end;
apply_tokens({Y,_,_},Time,Utime,[{weeknumber,Week},{weekday,Day}|Elements]) ->
	if
		Day =< 0 ->
			throw ({error,{day_too_low,Day }});
		Day >= 8 ->
			throw ({error,{day_too_big,{Y,Day} }});
		true ->
			ok
	end,
	check_week_number(Y,Week),
	Date=calendar:gregorian_days_to_date(
		gregorian_days_of_iso_w01_1(Y) +
		(Week-1)*7 + Day - 1
	),
	apply_tokens(Date,Time,Utime,Elements);
apply_tokens({Y,_,_},Time,Utime,[{weeknumber,Week}|Elements]) ->
	check_week_number(Y,Week),
	Date=calendar:gregorian_days_to_date(
		gregorian_days_of_iso_w01_1(Y) +
		(Week-1)*7
	),
	apply_tokens(Date,Time,Utime,Elements);
apply_tokens({Year,_,_},Time,Utime,[{yearday,Day}|Elements]) ->
	case {calendar:is_leap_year(Year) , Day} of
		{true,Day} when Day>366
		-> throw({error, {day_too_big, {Year, Day}}});
		{false, Day} when Day>365
		-> throw({error, {day_too_big, {Year, Day}}});
		{_, Day} when Day<1
		-> throw({error, {day_too_small, {Year, Day}}});
		_ -> ok
	end,
	Date=
		calendar:gregorian_days_to_date(
			calendar:date_to_gregorian_days({Year,1,1})+Day-1),
	apply_tokens(Date,Time,Utime,Elements);
apply_tokens(_,_,_,[Element|_]) ->
	throw({unknown_token,Element});
apply_tokens(Date,T,U,[]) ->
	{Date,T,U}.

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
            throw ({error,{day_too_low,Day }});
        Day >= 8 ->
            throw ({error,{day_too_big,{Y,Day} }});
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
            -> throw({error, {day_too_big, {Year, Day}}});
        {false, Day} when Day>365
            -> throw({error, {day_too_big, {Year, Day}}});
        {_, Day} when Day<1
            -> throw({error, {day_too_small, {Year, Day}}});
        _ -> ok
    end,
    Date=
        calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days({Year,1,1})+Day-1),
    apply_date_tokens(Date,Elements);
apply_date_tokens(_,[Element|_]) ->
    throw({unknown_token,Element});
apply_date_tokens(Date,[]) ->
    Date.

apply_time_tokens(Date,{_,M,S},U,[{hour,H}|Elements]) ->
	apply_time_tokens(Date,{H,M,S},U,Elements);
apply_time_tokens(Date,{H,_,S},U,[{minute,M}|Elements]) ->
	apply_time_tokens(Date,{H,M,S},U,Elements);
apply_time_tokens(Date,{H,M,_},U,[{second,S}|Elements]) ->
	apply_time_tokens(Date,{H,M,S},U,Elements);
apply_time_tokens(Date,OldTime,OldU,[{frac,Base,Numbers}|Elements]) ->
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
	apply_time_tokens(Date,NewTime,Microseconds,Elements);
apply_time_tokens(_,_,_,[Element|_]) ->
	throw({unknown_token,Element});
apply_time_tokens(Date,T,U,[]) ->
	{Date,T,U}.


check_week_number(Y,Week) ->
		{_, LastWeekNumber} = calendar:iso_week_number(
			calendar:gregorian_days_to_date(
				gregorian_days_of_iso_w01_1(Y+1)-1
			)
		),
		if
			Week < 1 ->
				throw ({error,{week_too_small,{Y,Week} }});
			Week > LastWeekNumber ->
				throw ({error,{week_too_big,{Y,Week} }});
			true -> ok
		end.



%%--------------------------------------------------------------------
%% @doc:	Parses time in specified format
%% @end
%%--------------------------------------------------------------------

-spec(parse_localtime(Format::localtime_format(), Value::nonempty_string() )
			-> {Time::calendar:time(), MicroSec::non_neg_integer(), TD::time_difference()}).

parse_localtime(Format, Value ) ->
	throw({error, {wrong_format,{Format, Value}}}).

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

%time_adjust_ret({{Hour,Minute,Sec},MicroSec},{HDif,MDif}) ->
	%{{Hour+HDif,Minute+MDif,Sec},MicroSec,{HDif,MDif}}.

%prepare_frac_time(Seconds, Frac, Base) ->
%	{FracVal,[]}=string:to_float("0."++Frac),
%	Diff=FracVal*Base,
%	Time=calendar:seconds_to_time(Seconds + trunc(Diff)),
%	{Time, round(Diff*1000000) rem 1000000}.


%% vim: set ts=2 sw=2 ai invlist si cul nu:
