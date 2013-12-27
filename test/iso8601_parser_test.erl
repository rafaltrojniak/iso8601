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

parse_parser_test_() ->
    [
        % Separators and designators
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_century_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_century_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_century_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_century_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_extend_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_extend_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_extend_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_extend_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_month_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_month_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_month_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_month_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_year_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_year_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(calendar_year_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(calendar_year_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_extended_1,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_extended_1,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_extended_2,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_extended_2,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_week,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_week,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_weekday,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_weekday,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_weekday_extended,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_weekday_extended,lexer))),
		?_assertEqual({ok,iso8601_phases:get_parse_date_stage(ordinal_week_extended,parser)},
			iso8601_parser:parse(iso8601_phases:get_parse_date_stage(ordinal_week_extended,lexer)))
    ].
