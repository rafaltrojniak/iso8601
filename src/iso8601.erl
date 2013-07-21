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
-export([parse_iso8601_date_calendar/1,
	parse_iso8601_date_orginal/1]).

%%--------------------------------------------------------------------
%% @doc:	Parses iso8601 caldendar dates as specified in standard on section 4.12
%% @spec:	function(binary()) -> return type.
%% @end
%%--------------------------------------------------------------------

-spec(parse_iso8601_date_calendar(Date::binary()|nonempty_string()) -> Date::calendar:date()).

parse_iso8601_date_calendar(Date)
		when is_binary(Date) ->
	parse_iso8601_date_calendar(binary_to_list(Date));
% Section 4.1.2.4 a - day extended
parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,$-,M1,M2,$-,D1,D2]) ->
	parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,M1,M2,D1,D2]) ;
% Section 4.1.2.4 b - month
parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,$-,M1,M2]) ->
	parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,M1,M2,$0,$1]) ;
% Section 4.1.2.4 c - year
parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5]) ->
	parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,$0,$1,$0,$1]) ;
% Section 4.1.2.4 d - century
parse_iso8601_date_calendar([$+,Y1,Y2,Y3]) ->
	parse_iso8601_date_calendar([$+,Y1,Y2,Y3,$0,$1,$0,$1,$0,$1]) ;
% Section 4.1.2.4 b - day basic
parse_iso8601_date_calendar([$+,Y1,Y2,Y3,Y4,Y5,M1,M2,D1,D2])
		when
	is_integer(Y1) andalso is_integer(Y2) andalso is_integer(Y3) andalso
	is_integer(Y4) andalso is_integer(Y5) andalso
	is_integer(M1) andalso is_integer(M2) andalso
	is_integer(D1) andalso is_integer(D2)
	andalso Y1 >= $0 andalso Y1 =< $9
	andalso Y2 >= $0 andalso Y2 =< $9
	andalso Y3 >= $0 andalso Y3 =< $9
	andalso Y4 >= $0 andalso Y4 =< $9
	andalso Y5 >= $0 andalso Y5 =< $9
	andalso M1 >= $0 andalso M1 =< $9
	andalso M2 >= $0 andalso M2 =< $9
	andalso D1 >= $0 andalso D1 =< $9
	andalso D2 >= $0 andalso D2 =< $9
	->
		{Year,[]}=string:to_integer([Y1,Y2,Y3,Y4,Y5]),
		{Month,[]}=string:to_integer([M1,M2]),
		{Day,[]}=string:to_integer([D1,D2]),
		Date={Year, Month, Day},
		case Date of
			{_Year, Month, _Day} when Month =< 0 ->
				throw ({failed_too_parse, month_too_low, Date});
			{_Year, Month, _Day} when Month > 12 ->
				throw ({failed_too_parse, month_too_big, Date});
			{_Year, _Month, Day} when Day =< 0 ->
				throw ({failed_too_parse, day_too_low, Date});
			{_Year, _Month, Day} ->
				LastDay=calendar:last_day_of_the_month(Year, Month),
				if
					LastDay < Day ->
						throw ({failed_too_parse, day_too_big, Date});
					true -> Date
				end;
			Date -> Date
		end;
% Section 4.1.2.3 c - century
parse_iso8601_date_calendar([Y1,Y2]) ->
	parse_iso8601_date_calendar([$+,$0,Y1,Y2,$0,$1,$-,$0,$1,$-,$0,$1]);
% Section 4.1.2.3 b - year
parse_iso8601_date_calendar([Y1,Y2,Y3,Y4])  ->
	parse_iso8601_date_calendar([$+,$0,Y1,Y2,Y3,Y4,$-,$0,$1,$-,$0,$1]);
% Section 4.1.2.3 a - month
parse_iso8601_date_calendar([Y1,Y2,Y3,Y4,$-,M1,M2])  ->
	parse_iso8601_date_calendar([$+,$0,Y1,Y2,Y3,Y4,$-,M1,M2,$-,$0,$1]);
% Section 4.1.2.2 extended
parse_iso8601_date_calendar([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2])  ->
	parse_iso8601_date_calendar([$+,$0,Y1,Y2,Y3,Y4,M1,M2,D1,D2]);
% Section 4.1.2.2 basic
parse_iso8601_date_calendar([Y1,Y2,Y3,Y4,M1,M2,D1,D2])  ->
	parse_iso8601_date_calendar([$+,$0,Y1,Y2,Y3,Y4,M1,M2,D1,D2]);
parse_iso8601_date_calendar(String)
		when is_list(String) ->
	throw({failed_too_parse,wrong_format,String}).

-spec(parse_iso8601_date_orginal(DateDay :: binary()) -> GregorianDays::integer()
		;	(DateDay::list(integer())) -> GregorianDays::integer()	).
% Section 4.1.3.2 basic
parse_iso8601_date_orginal(DateDay)
	when is_binary(DateDay)	->
	parse_iso8601_date_orginal(binary_to_list(DateDay));
parse_iso8601_date_orginal([Y1,Y2,Y3,Y4,D1,D2,D3])  ->
	parse_iso8601_date_orginal([$+,$0,Y1,Y2,Y3,Y4,D1,D2,D3]) ;
% Section 4.1.3.2 extended
parse_iso8601_date_orginal([Y1,Y2,Y3,Y4,$-,D1,D2,D3])  ->
	parse_iso8601_date_orginal([$+,$0,Y1,Y2,Y3,Y4,D1,D2,D3]) ;
% Section 4.1.3.3 basic
parse_iso8601_date_orginal([$+,Y1,Y2,Y3,Y4,Y5,D1,D2,D3])  ->
	parse_iso8601_date_orginal([$+,Y1,Y2,Y3,Y4,Y5,$-,D1,D2,D3]) ;
% Section 4.1.3.3 extended
parse_iso8601_date_orginal([$+,Y1,Y2,Y3,Y4,Y5,$-,D1,D2,D3])
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
		{Year,[]}=string:to_integer([Y1,Y2,Y3,Y4,Y5]),
		{Day,[]}=string:to_integer([D1,D2,D3]),
		case {calendar:is_leap_year(Year) , Day} of
			{true,Day} when Day>366
			-> throw({day_too_big,Day});
			{false,Day} when Day>365
			-> throw({day_too_big,Day});
			{_,Day} when Day<1
			-> throw({day_too_small,Day});
			_ -> ok
		end,
		Days=calendar:date_to_gregorian_days(Year, 1, 1),
		Days+Day-1;
parse_iso8601_date_orginal(String)
		when is_list(String) ->
	throw({failed_too_parse,String}).


%parse_iso8601_date_week
%parse_iso8601_time_local
%parse_iso8601_time_midnight
%parse_iso8601_time_UTC
%parse_iso8601_datetime

%% vim: set ts=2 sw=2 ai invlist si cul nu:
