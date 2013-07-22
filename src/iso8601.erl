%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		iso 8601 parsing tools
%%% @end
%%% Created :  pon lip 15 22:19:15 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601).

%% API
-export([parse_date/1,
	format_date/1,
	format_date/2
]).

%%--------------------------------------------------------------------
%% @doc:	Parses iso8601 caldendar dates as specified in standard on section 4.12
%% @spec:	function(binary()) -> return type.
%% @end
%%--------------------------------------------------------------------

-spec(parse_date(Date::binary()|nonempty_string()) -> Date::calendar:date()).

parse_date(Date)
		when is_binary(Date) ->
	parse_date(binary_to_list(Date));
% Section 4.1.2.3 c - century
parse_date([Y1,Y2])
		when
	is_integer(Y1) andalso is_integer(Y2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	->
	parse_date([Y1,Y2,$0,$1,$-,$0,$1,$-,$0,$1]);
% Section 4.1.2.3 b - year
parse_date([Y1,Y2,Y3,Y4])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso is_integer(Y3)
	andalso is_integer(Y4)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,$-,$0,$1,$-,$0,$1]);
% Section 4.1.2.3 a - month
parse_date([Y1,Y2,Y3,Y4,$-,M1,M2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso 
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(M1) andalso is_integer(M2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso M1 >= $0 andalso M1 =< $9
	andalso M2 >= $0 andalso M2 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,$-,M1,M2,$-,$0,$1]);
% Section 4.1.2.2 extended
parse_date([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso 
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(M1) andalso is_integer(M2) andalso
	is_integer(D1) andalso is_integer(D2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso M1 >= $0 andalso M1 =< $9
	andalso M2 >= $0 andalso M2 =< $9
	andalso D1 >= $0 andalso D1 =< $9
	andalso D2 >= $0 andalso D2 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,M1,M2,D1,D2]);
% Section 4.1.2.2 basic
parse_date([Y1,Y2,Y3,Y4,M1,M2,D1,D2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(M1) andalso is_integer(M2) andalso
	is_integer(D1) andalso is_integer(D2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso M1 >= $0 andalso M1 =< $9
	andalso M2 >= $0 andalso M2 =< $9
	andalso D1 >= $0 andalso D1 =< $9
	andalso D2 >= $0 andalso D2 =< $9
	->
		{Year,[]}=string:to_integer([Y1,Y2,Y3,Y4]),
		{Month,[]}=string:to_integer([M1,M2]),
		{Day,[]}=string:to_integer([D1,D2]),
		Date={Year, Month, Day},
		case Date of
			{_Year, Month, _Day} when Month =< 0 ->
				throw ({error, { month_too_low, Date}});
			{_Year, Month, _Day} when Month > 12 ->
				throw ({error, { month_too_big, Date}});
			{_Year, _Month, Day} when Day =< 0 ->
				throw ({error, { day_too_low, Date}});
			{_Year, _Month, Day} ->
				LastDay=calendar:last_day_of_the_month(Year, Month),
				if
					LastDay < Day ->
						throw ({error, { day_too_big, Date}});
					true -> Date
				end
		end;

% Ordinal date formats

% Section 4.1.3.2 extended
parse_date([Y1,Y2,Y3,Y4,$-,D1,D2,D3])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso is_integer(Y3) andalso
	is_integer(Y4) andalso
	is_integer(D1) andalso is_integer(D2) andalso is_integer(D3)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso D1 >= $0 andalso D1 =< $9
	andalso D2 >= $0 andalso D2 =< $9
	andalso D3 >= $0 andalso D3 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,D1,D2,D3]) ;
% Section 4.1.3.2 basic
parse_date([Y1,Y2,Y3,Y4,D1,D2,D3])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso is_integer(Y3) andalso
	is_integer(Y4) andalso
	is_integer(D1) andalso is_integer(D2) andalso is_integer(D3)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso D1 >= $0 andalso D1 =< $9
	andalso D2 >= $0 andalso D2 =< $9
	andalso D3 >= $0 andalso D3 =< $9
	->
		{Year,[]}=string:to_integer([Y1,Y2,Y3,Y4]),
		{Day,[]}=string:to_integer([D1,D2,D3]),
		case {calendar:is_leap_year(Year) , Day} of
			{true,Day} when Day>366
			-> throw({error, {day_too_big, {Year, Day}}});
			{false, Day} when Day>365
			-> throw({error, {day_too_big, {Year, Day}}});
			{_, Day} when Day<1
			-> throw({error, {day_too_small, {Year, Day}}});
			_ -> ok
		end,
		Days=calendar:date_to_gregorian_days(Year, 1, 1),
		calendar:gregorian_days_to_date(Days+Day-1);
% Week date formats
% 4.1.4.3 Basic format: YYYYWww example: 1985W15
parse_date([Y1,Y2,Y3,Y4,$W,W1,W2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(W1) andalso is_integer(W2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso W1 >= $0 andalso W1 =< $9
	andalso W2 >= $0 andalso W2 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,$-,$W,W1,W2,$-,$1]);
% 4.1.4.3 Extended format: YYYY-Www example: 1985-W15
parse_date([Y1,Y2,Y3,Y4,$-,$W,W1,W2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(W1) andalso is_integer(W2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso W1 >= $0 andalso W1 =< $9
	andalso W2 >= $0 andalso W2 =< $9
	->
	parse_date([Y1,Y2,Y3,Y4,$-,$W,W1,W2,$-,$1]);
% 4.1.4.2 Basic format: YYYYWwwD example: 1985W155
parse_date([Y1,Y2,Y3,Y4,$W,W1,W2,D1])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(W1) andalso is_integer(W2)
	andalso is_integer(D1)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso W1 >= $0 andalso W1 =< $9
	andalso W2 >= $0 andalso W2 =< $9
	andalso D1 >= $1 andalso D1 =< $7
	->
	parse_date([Y1,Y2,Y3,Y4,$-,$W,W1,W2,$-,D1]);
% 4.1.4.2 Extended format: YYYY-Www-D example: 1985-W15-5
parse_date([Y1,Y2,Y3,Y4,$-,$W,W1,W2,$-,D1])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso
	is_integer(Y3) andalso is_integer(Y4) andalso
	is_integer(W1) andalso is_integer(W2)
	andalso is_integer(D1)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso W1 >= $0 andalso W1 =< $9
	andalso W2 >= $0 andalso W2 =< $9
	andalso D1 >= $1 andalso D1 =< $7
	->
		{Year,[]}=string:to_integer([Y1,Y2,Y3,Y4]),
		{Week,[]}=string:to_integer([W1,W2]),
		Day=D1-$0,

		{_, LastWeekNumber} = calendar:iso_week_number(
			calendar:gregorian_days_to_date(
				gregorian_days_of_iso_w01_1(Year+1)-1
			)
		),
		% Checks
		if
			Week <1 ->
				throw({error, {week_too_small,{Year,Week}}});
			Week > LastWeekNumber ->
				throw({error, {week_too_big,{Year,Week}}});
			true ->
				calendar:gregorian_days_to_date(
					gregorian_days_of_iso_w01_1(Year) +
					(Week-1)*7 +
					Day - 1
				)
		end;
parse_date(String)
		when is_list(String) ->
	throw({error, {wrong_format,String}}).

%%--------------------------------------------------------------------
%% @doc:	Formats date in calendar_extended format
%% @spec:	format_date(Date::calendar:date() ) -> nonempty_string().
%% @end
%%--------------------------------------------------------------------

-spec(format_date(calendar:date()) -> nonempty_string() ).

format_date(Date) ->
	format_date(Date,calendar_extended).

%%--------------------------------------------------------------------
%% @doc:	Formats date in specific format
%% @spec:	format_date(Date::calendar:date(), Type ) -> nonempty_string().
%% @end
%%--------------------------------------------------------------------

-spec(format_date(calendar:date(), Type) -> nonempty_string() 
	when Type::
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
		week_extended
	).

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

%TODO parse_time_local
%TODO parse_time_midnight
%TODO parse_time_UTC
%TODO parse_datetime

% Helper functions
%
%%
%% The Gregorian days of the iso week 01 day 1 for a given year.
%% copied from calendar module from stdlib
%%
-spec gregorian_days_of_iso_w01_1(calendar:year()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
    D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
    DOW = calendar:day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
	D0101 - DOW + 1;
    true ->
	D0101 + 7 - DOW + 1
    end.

%% vim: set ts=2 sw=2 ai invlist si cul nu:
