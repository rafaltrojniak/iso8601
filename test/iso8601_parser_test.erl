%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Testing date parsing library
%%% @end
%%% Created :  wto lip 02 23:34:20 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601_parser_test).

-include_lib("eunit/include/eunit.hrl").

parse_date_test_() ->
    [
        % Separators and designators
		?_assertEqual({ok,iso8601_phases:get_date(calendar_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_century_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_century_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_century_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_century_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_extend_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_extend_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_extend_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_extend_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_month_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_month_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_month_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_month_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_year_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_year_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(calendar_year_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(calendar_year_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(ordinal_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(ordinal_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(ordinal_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(ordinal_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(ordinal_extended_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(ordinal_extended_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(ordinal_extended_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(ordinal_extended_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(week,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(week,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(weekday,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(weekday,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(weekday_extended,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(weekday_extended,lexer))),
		?_assertEqual({ok,iso8601_phases:get_date(week_extended,parser)},
			iso8601_parser:parse(iso8601_phases:get_date(week_extended,lexer)))
    ].

parse_time_test_() ->
    [
?_assertEqual(iso8601_phases:get_time(general_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_extended_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_extended_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_extended_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_extended_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_extended_frac_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_extended_frac_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_extended_frac_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_extended_frac_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_frac_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_frac_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_frac_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_frac_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_hour_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_hour_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_hour_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_hour_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_hour_frac_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_hour_frac_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_hour_frac_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_hour_frac_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_extended_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_extended_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_extended_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_extended_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_extended_frac_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_extended_frac_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_extended_frac_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_extended_frac_2,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_frac_1,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_frac_1,lexer)))),
?_assertEqual(iso8601_phases:get_time(general_minute_frac_2,parser)
    ,iso8601_parser:parse(element(2,iso8601_phases:get_time(general_minute_frac_2,lexer))))
    ].

parse_localtime_test_() ->
    lists:map(
        fun(X) ->
            LexElements=element(2,iso8601_phases:get_localtime(X,lexer)),
            ParserResult=iso8601_phases:get_localtime(X,parser),
            ?_assertEqual(ParserResult
                ,iso8601_parser:parse(LexElements))
        end,
    [
        general_1,
        general_2,
        general_3,
        general_4,
        general_5,
        general_extended_1,
        general_extended_frac_1,
        general_frac_1,
        general_hour_1,
        general_hour_frac_1,
        general_minute_1,
        general_minute_extended_1,
        general_minute_extended_frac_1,
        general_minute_frac_1
    ]).

parse_datetime_test_() ->
    lists:map(
        fun(X) ->
            LexElements=element(2,iso8601_phases:get_datetime(X,lexer)),
            ParserResult=iso8601_phases:get_datetime(X,parser),
            ?_assertEqual(ParserResult
                ,iso8601_parser:parse(LexElements))
        end,
    [
        general_1,
        general_2,
        general_3,
        general_4
    ]).

parse_localdatetime_test_() ->
    lists:map(
        fun(X) ->
            LexElements=element(2,iso8601_phases:get_localdatetime(X,lexer)),
            ParserResult=iso8601_phases:get_localdatetime(X,parser),
            ?_assertEqual(ParserResult
                ,iso8601_parser:parse(LexElements))
        end,
    [
        general_1,
        general_2,
        general_3,
        general_4
    ]).
