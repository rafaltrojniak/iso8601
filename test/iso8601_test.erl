%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Testing date parsing library
%%% @end
%%% Created :  wto lip 02 23:34:20 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601_test).

-include_lib("eunit/include/eunit.hrl").

%OLD parse_date_test_() ->
%OLD 	[
%OLD 		% Standard checks
%OLD
%OLD 		% Format autodetection checks
%OLD 		?_assertEqual({1,1,1}, iso8601:parse_date(<<"00">>) ),
%OLD 		?_assertEqual({1,1,1}, iso8601:parse_date("00") ),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000") ),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01") ),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01-01") ),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("00000101") ),
%OLD
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000001") ),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-001") ),
%OLD
%OLD 		?_assertEqual({2001,1,1}, iso8601:parse_date("20") ),
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013") ),
%OLD 		?_assertEqual({2013,7,1}, iso8601:parse_date("2013-07") ),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-07-21") ),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("20130721") ),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013202") ),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-202") ),
%OLD
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013001") ),
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013-001") ),
%OLD
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013W297") ),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-W29-7") ),
%OLD
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W021") ),
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02-1") ),
%OLD
%OLD 		?_assertEqual({2013,7,15}, iso8601:parse_date("2013W29") ),
%OLD 		?_assertEqual({2013,7,15}, iso8601:parse_date("2013-W29") ),
%OLD
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W02") ),
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02") ),
%OLD
%OLD 		% Format specific checks
%OLD 		?_assertEqual({1,1,1}, iso8601:parse_date(<<"00">>, calendar_century )),
%OLD 		?_assertEqual({1,1,1}, iso8601:parse_date("00", calendar_century)),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000", calendar_year )),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01", calendar_month )),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01-01", calendar_extended  )),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("00000101" ,calendar )),
%OLD
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000001", ordinal )),
%OLD 		?_assertEqual({0,1,1}, iso8601:parse_date("0000-001", ordinal_extended )),
%OLD
%OLD 		?_assertEqual({2001,1,1}, iso8601:parse_date("20", calendar_century )),
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013", calendar_year )),
%OLD 		?_assertEqual({2013,7,1}, iso8601:parse_date("2013-07", calendar_month )),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-07-21", calendar_extended )),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("20130721",calendar )),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013202", ordinal )),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-202", ordinal_extended )),
%OLD
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013001", ordinal )),
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013-001", ordinal_extended )),
%OLD
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013W297", weekday )),
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-W29-7", weekday_extended )),
%OLD
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W021", weekday )),
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02-1", weekday_extended )),
%OLD
%OLD 		?_assertEqual({2013,7,15}, iso8601:parse_date("2013W29", week )),
%OLD 		?_assertEqual({2013,7,15}, iso8601:parse_date("2013-W29", week_extended )),
%OLD
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W02", week )),
%OLD 		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02", week_extended )),
%OLD
%OLD 		% Binary format pasing
%OLD 		?_assertEqual({2013,7,21}, iso8601:parse_date(<<"2013-07-21">>) ),
%OLD
%OLD 		% Wrong format checks
%OLD 		%?_assertThrow({error, {wrong_format,""}}, iso8601:parse_date("") ),
%OLD 		%?_assertThrow({error, {wrong_format,"poc"}}, iso8601:parse_date("poc") ),
%OLD 		%?_assertThrow({error, {wrong_format,_}}, iso8601:parse_date("2013X01X01") ),
%OLD 		?_assertThrow({error, _}, iso8601:parse_date("2013",calendar) ),
%OLD 		?_assertThrow({error, _}, iso8601:parse_date("20130101",calendar_extended) ),
%OLD 		?_assertThrow({error, _}, iso8601:parse_date("2013-01-01",calendar) ),
%OLD
%OLD
%OLD 		% Standard day/month too_big/too_low checks
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-01-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-02-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-03-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-04-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-05-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-06-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-07-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-08-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-09-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-10-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-11-00") ),
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2013-12-00") ),
%OLD 		?_assertThrow({error, {month_too_big,_}}, iso8601:parse_date("2013-13-00") ),
%OLD 		?_assertThrow({error, {month_too_low,_}}, iso8601:parse_date("2013-00-01") ),
%OLD
%OLD 		?_assertThrow({error, {day_too_small,{2011,0}}}, iso8601:parse_date("2011-000") ),
%OLD 		?_assertThrow({error, {day_too_big,{2011,600}}}, iso8601:parse_date("2011-600") ),
%OLD
%OLD 		?_assertEqual({2013,1,1}, iso8601:parse_date("2013-01-01") ),
%OLD 		?_assertEqual({2013,2,1}, iso8601:parse_date("2013-02-01") ),
%OLD 		?_assertEqual({2013,3,1}, iso8601:parse_date("2013-03-01") ),
%OLD 		?_assertEqual({2013,4,1}, iso8601:parse_date("2013-04-01") ),
%OLD 		?_assertEqual({2013,5,1}, iso8601:parse_date("2013-05-01") ),
%OLD 		?_assertEqual({2013,6,1}, iso8601:parse_date("2013-06-01") ),
%OLD 		?_assertEqual({2013,7,1}, iso8601:parse_date("2013-07-01") ),
%OLD 		?_assertEqual({2013,8,1}, iso8601:parse_date("2013-08-01") ),
%OLD 		?_assertEqual({2013,9,1}, iso8601:parse_date("2013-09-01") ),
%OLD 		?_assertEqual({2013,10,1}, iso8601:parse_date("2013-10-01") ),
%OLD 		?_assertEqual({2013,11,1}, iso8601:parse_date("2013-11-01") ),
%OLD 		?_assertEqual({2013,12,1}, iso8601:parse_date("2013-12-01") ),
%OLD 		?_assertThrow({error, {month_too_big,_}}, iso8601:parse_date("2013-13-01") ),
%OLD
%OLD 		?_assertEqual({2013,1,31}, iso8601:parse_date("2013-01-31") ),
%OLD 		?_assertEqual({2013,2,28}, iso8601:parse_date("2013-02-28") ),
%OLD 		?_assertEqual({2013,3,31}, iso8601:parse_date("2013-03-31") ),
%OLD 		?_assertEqual({2013,4,30}, iso8601:parse_date("2013-04-30") ),
%OLD 		?_assertEqual({2013,5,31}, iso8601:parse_date("2013-05-31") ),
%OLD 		?_assertEqual({2013,6,30}, iso8601:parse_date("2013-06-30") ),
%OLD 		?_assertEqual({2013,7,31}, iso8601:parse_date("2013-07-31") ),
%OLD 		?_assertEqual({2013,8,31}, iso8601:parse_date("2013-08-31") ),
%OLD 		?_assertEqual({2013,9,30}, iso8601:parse_date("2013-09-30") ),
%OLD 		?_assertEqual({2013,10,31}, iso8601:parse_date("2013-10-31") ),
%OLD 		?_assertEqual({2013,11,30}, iso8601:parse_date("2013-11-30") ),
%OLD 		?_assertEqual({2013,12,31}, iso8601:parse_date("2013-12-31") ),
%OLD 		?_assertThrow({error, {month_too_big,_}}, iso8601:parse_date("2013-13-30") ),
%OLD
%OLD
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-01-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-02-29") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-03-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-04-31") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-05-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-06-31") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-07-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-08-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-09-31") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-10-32") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-11-31") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2013-12-32") ),
%OLD 		?_assertThrow({error, {month_too_big,_}}, iso8601:parse_date("2013-13-32") ),
%OLD
%OLD 		% Week day tests
%OLD 		?_assertThrow({error, {week_too_small,_}}, iso8601:parse_date("2011-W00-1") ),
%OLD 		?_assertThrow({error, {week_too_big,_}}, iso8601:parse_date("2011-W99-1") ),
%OLD 		?_assertEqual({2009,12,28}, iso8601:parse_date("2009-W53-1") ),
%OLD 		?_assertEqual({2010,12,27}, iso8601:parse_date("2010-W52-1") ),
%OLD 		?_assertEqual({2011,12,26}, iso8601:parse_date("2011-W52-1") ),
%OLD 		?_assertEqual({2012,1,1}, iso8601:parse_date("2011-W52-7") ), % Skipping to next year
%OLD 		?_assertEqual({2012,12,24}, iso8601:parse_date("2012-W52-1") ),
%OLD 		?_assertEqual({2013,12,23}, iso8601:parse_date("2013-W52-1") ),
%OLD 		?_assertEqual({2012,12,24}, iso8601:parse_date("2012-W52") ),
%OLD 		?_assertEqual({2013,12,23}, iso8601:parse_date("2013-W52") ),
%OLD 		?_assertEqual({2012,12,24}, iso8601:parse_date("2012W52") ),
%OLD 		?_assertEqual({2013,12,23}, iso8601:parse_date("2013W52") ),
%OLD 		?_assertThrow({error, {week_too_big,_}}, iso8601:parse_date("2009-W54-1") ),
%OLD 		?_assertThrow({error, {week_too_big,_}}, iso8601:parse_date("2012-W53-1") ),
%OLD 		?_assertThrow({error, {week_too_big,_}}, iso8601:parse_date("2013-W53-1") ),
%OLD
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2011-W11-0") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2011-W11-8") ),
%OLD
%OLD 		% leep day check
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2012-02-00") ),
%OLD 		?_assertEqual({2012,2,1}, iso8601:parse_date("2012-02-01") ),
%OLD 		?_assertEqual({2012,2,29}, iso8601:parse_date("2012-02-29") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2012-02-30") ),
%OLD 		?_assertEqual({2012,12,31}, iso8601:parse_date("2012-366") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2012-367") ),
%OLD
%OLD 		?_assertThrow({error, {day_too_low,_}}, iso8601:parse_date("2011-02-00") ),
%OLD 		?_assertEqual({2011,2,1}, iso8601:parse_date("2011-02-01") ),
%OLD 		?_assertEqual({2011,2,28}, iso8601:parse_date("2011-02-28") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2011-02-29") ),
%OLD 		?_assertEqual({2011,12,31}, iso8601:parse_date("2011-365") ),
%OLD 		?_assertThrow({error, {day_too_big,_}}, iso8601:parse_date("2011-366") )
%OLD 	].
%OLD
%OLD format_date_test_() ->
%OLD 	[
%OLD 		?_assertEqual("2011-12-31", iso8601:format_date({2011,12,31}) ),
%OLD 		?_assertEqual("20111231", iso8601:format_date({2011,12,31}, calendar) ),
%OLD 		?_assertEqual("2011-12-31", iso8601:format_date({2011,12,31}, calendar_extended) ),
%OLD 		?_assertEqual("2011-12", iso8601:format_date({2011,12,31}, calendar_month) ),
%OLD 		?_assertEqual("2011", iso8601:format_date({2011,12,31}, calendar_year) ),
%OLD 		?_assertEqual("20", iso8601:format_date({2011,12,31}, calendar_century) ),
%OLD 		?_assertEqual("2011365", iso8601:format_date({2011,12,31}, ordinal) ),
%OLD 		?_assertEqual("2011-365", iso8601:format_date({2011,12,31}, ordinal_extended) ),
%OLD 		?_assertEqual("2011W526", iso8601:format_date({2011,12,31}, weekday) ),
%OLD 		?_assertEqual("2011-W52-6", iso8601:format_date({2011,12,31}, weekday_extended) ),
%OLD 		?_assertEqual("2011W52", iso8601:format_date({2011,12,31}, week) ),
%OLD 		?_assertEqual("2011-W52", iso8601:format_date({2011,12,31}, week_extended) ),
%OLD
%OLD
%OLD 		?_assertEqual("0011-01-02", iso8601:format_date({11,1,2}) ),
%OLD 		?_assertEqual("00110102", iso8601:format_date({11,1,2}, calendar) ),
%OLD 		?_assertEqual("0011-01-02", iso8601:format_date({11,1,2}, calendar_extended) ),
%OLD 		?_assertEqual("0011-01", iso8601:format_date({11,1,31}, calendar_month) ),
%OLD 		?_assertEqual("0011", iso8601:format_date({11,1,31}, calendar_year) ),
%OLD 		?_assertEqual("20", iso8601:format_date({2011,1,31}, calendar_century) ),
%OLD 		?_assertEqual("2011031", iso8601:format_date({2011,1,31}, ordinal) ),
%OLD 		?_assertEqual("2011-031", iso8601:format_date({2011,1,31}, ordinal_extended) ),
%OLD 		?_assertEqual("2011W011", iso8601:format_date({2011,1,3}, weekday) ),
%OLD 		?_assertEqual("2011-W01-1", iso8601:format_date({2011,1,3}, weekday_extended) ),
%OLD 		?_assertEqual("2011W01", iso8601:format_date({2011,1,3}, week) ),
%OLD 		?_assertEqual("2011-W01", iso8601:format_date({2011,1,3}, week_extended) ),
%OLD 		?_assertThrow({error, {unknown_format,wtf}},iso8601:format_date({2011,1,3}, wtf)),
%OLD 		?_assertThrow({error, {unknown_data,wtf}},iso8601:format_date(wtf, calendar)),
%OLD 		?_assertThrow({error, {unknown_data,wtf}},iso8601:format_date(wtf, wtf2))
%OLD
%OLD 	].
%OLD
%OLD parse_time_test_() ->
%OLD 	[
%OLD
%OLD 		?_assertEqual({{23,05,50},0},iso8601:parse_time( "230550") ),
%OLD 		?_assertEqual({{23,05,50},0},iso8601:parse_time( "23:05:50") ),
%OLD 		?_assertEqual({{23,05,0},0},iso8601:parse_time( "2305") ),
%OLD 		?_assertEqual({{23,05,0},0},iso8601:parse_time( "23:05") ),
%OLD 		?_assertEqual({{23,0,0},0},iso8601:parse_time( "23") ),
%OLD 		?_assertEqual({{23,05,50},120000},iso8601:parse_time( "230550,12") ),
%OLD 		?_assertEqual({{23,05,50},120000},iso8601:parse_time( "23:05:50,12") ),
%OLD 		?_assertEqual({{23,05,7},200000},iso8601:parse_time( "2305,12") ),
%OLD 		?_assertEqual({{23,05,7},200000},iso8601:parse_time( "23:05,12") ),
%OLD 		?_assertEqual({{23,07,12},0},iso8601:parse_time( "23,12") ),
%OLD 		?_assertEqual({{23,05,50},0},iso8601:parse_time( "T230550") ),
%OLD 		?_assertEqual({{23,05,50},0},iso8601:parse_time( "T23:05:50") ),
%OLD 		?_assertEqual({{23,05,0},0},iso8601:parse_time( "T2305") ),
%OLD 		?_assertEqual({{23,05,0},0},iso8601:parse_time( "T23:05") ),
%OLD 		?_assertEqual({{23,0,0},0},iso8601:parse_time( "T23") ),
%OLD 		?_assertEqual({{23,05,50},120000},iso8601:parse_time( "T230550,12") ),
%OLD 		?_assertEqual({{23,05,50},120000},iso8601:parse_time( "T23:05:50,12") ),
%OLD 		?_assertEqual({{23,05,7},200000},iso8601:parse_time( "T2305,12") ),
%OLD 		?_assertEqual({{23,05,7},200000},iso8601:parse_time( "T23:05,12") ),
%OLD 		?_assertEqual({{23,07,12},0},iso8601:parse_time( "T23,12") )
%OLD
%OLD 	].
%OLD
%OLD parse_time_presision_test_() ->
%OLD 	[
%OLD
%OLD 		?_assertEqual({{23,05,50},123400},iso8601:parse_time( "23:05:50,1234") ),
%OLD 		?_assertEqual({{23,05,50},123456},iso8601:parse_time( "23:05:50,123456") ),
%OLD 		?_assertEqual({{23,05,50},1},iso8601:parse_time( "23:05:50,000001") ),
%OLD 		?_assertEqual({{23,05,50},1},iso8601:parse_time( "23:05:50,0000010") ),
%OLD 		?_assertEqual({{23,05,50},1},iso8601:parse_time( "23:05:50,0000011") ),
%OLD 		?_assertEqual({{23,05,50},1},iso8601:parse_time( "23:05:50,0000014") ),
%OLD 		?_assertEqual({{23,05,50},2},iso8601:parse_time( "23:05:50,0000015") ),
%OLD 		?_assertEqual({{23,05,50},2},iso8601:parse_time( "23:05:50,0000019") ),
%OLD 		?_assertEqual({{23,05,50},2},iso8601:parse_time( "23:05:50,000002") ),
%OLD 		?_assertEqual({{23,05,50},999999},iso8601:parse_time( "23:05:50,999999") )
%OLD
%OLD 	].
%OLD
%OLD parse_time_rollover_test_() ->
%OLD 	[
%OLD 		?_assertEqual({{25,0,1},0},iso8601:parse_time( "25:00:01") )
%OLD 	].
%

parse_date_test() ->
    Input   = iso8601_phases:get_date(calendar_century_1, input),
    Lexer   = iso8601_phases:get_date(calendar_century_1, lexer),
    Parser  = iso8601_phases:get_date(calendar_century_1, parser),
    Value   = iso8601_phases:get_date(calendar_century_1, value),
    Format  = iso8601_phases:get_date(calendar_century_1, format),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), {ok,Lexer,1} end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(Lexer,_Lexer), {ok,Parser} end),
    meck:expect(iso8601,apply_date_tokens,fun(_Date,_Tokens) -> ?assertEqual(_Date,{0,1,1}), ?assertEqual(element(2,element(2,Parser)),_Tokens),Value end),
    ?assertEqual({Value,Format},iso8601:parse_date(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(iso8601),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_date_wrongformat_test() ->
    Input   = iso8601_phases:get_time(general_1, input),
    Lexer   = iso8601_phases:get_time(general_1, lexer),
    Parser  = iso8601_phases:get_time(general_1, parser),
    LexTokens=element(2,Lexer),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    ?assertThrow({error,{format_mismatch,time}},iso8601:parse_date(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

apply_date_tokens_test_() ->

    lists:map(
        fun(X) ->
            Value= iso8601_phases:get_date(X, value),
            Tokens=element(2,element(2,iso8601_phases:get_date(X, parser))),
            ?_assertEqual( Value, iso8601:apply_date_tokens({0,1,1},Tokens))
        end,
        [
            calendar_century_1,
            calendar_century_2,
            calendar_year_1,
            calendar_year_2,
            calendar_month_1,
            calendar_month_2,
            calendar_1,
            calendar_2,
            ordinal_1,
            ordinal_2,
            week,
            week_extended,
            weekday,
            weekday_extended
        ]).


format_date_test_() ->
    [
        ?_assertEqual(
            iso8601_phases:get_date(calendar_extend_1, input),
            iso8601:format_date(
                iso8601_phases:get_date(calendar_extend_1,value)
            ))
    ].

format_date_formatted_test_() ->
    lists:map(
        fun(X) ->
            Input= iso8601_phases:get_date(X, input),
            Value= iso8601_phases:get_date(X, value),
            Format=iso8601_phases:get_date(X, format),
            ?_assertEqual(Input, iso8601:format_date(Value, Format))
        end,
        [
            calendar_century_1,
            calendar_century_2,
            calendar_year_1,
            calendar_year_2,
            calendar_month_1,
            calendar_month_2,
            calendar_1,
            calendar_2,
            calendar_extend_1,
            calendar_extend_2,
            ordinal_1,
            ordinal_2,
            ordinal_extended_1,
            ordinal_extended_2,
            week,
            week_extended,
            weekday,
            weekday_extended
        ]).

apply_time_tokens_test_() ->
    lists:map(
        fun(X) ->
            Value = iso8601_phases:get_time(X,value),
            Parsed = element(2,element(2,element(2,iso8601_phases:get_time(X,parser)))),
            ?_assertEqual( Value, iso8601:apply_time_tokens({0,0,0},0,Parsed))
        end,
    [
        general_1 ,
        general_2 ,
        general_extended_1 ,
        general_extended_2 ,
        general_extended_frac_1 ,
        general_extended_frac_2 ,
        general_frac_1 ,
        general_frac_2 ,
        general_hour_1 ,
        general_hour_2 ,
        general_hour_frac_1 ,
        general_hour_frac_2 ,
        general_minute_1 ,
        general_minute_2 ,
        general_minute_extended_1 ,
        general_minute_extended_2 ,
        general_minute_extended_frac_1 ,
        general_minute_extended_frac_2 ,
        general_minute_frac_1 ,
        general_minute_frac_2
    ]).

parse_time_test() ->
    Input   = iso8601_phases:get_time(general_1, input),
    Lexer   = iso8601_phases:get_time(general_1, lexer),
    Parser  = iso8601_phases:get_time(general_1, parser),
    Value   = iso8601_phases:get_time(general_1, value),
    Format  = iso8601_phases:get_time(general_1, format),
    LexTokens=element(2,Lexer),
    Tokens  = element(2,element(2,element(2,Parser))),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    meck:expect(iso8601,apply_time_tokens,
                fun(_Time,_UTime,_Tokens) ->
                        ?assertEqual(_Time,{0,1,1}),
                        ?assertEqual(_UTime,0),
                        ?assertEqual(Tokens,_Tokens),Value end),
    ?assertEqual({Value,Format},iso8601:parse_time(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(iso8601),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_time_wrongform_test() ->
    Input   = iso8601_phases:get_localtime(general_1, input),
    Lexer   = iso8601_phases:get_localtime(general_1, lexer),
    Parser  = iso8601_phases:get_localtime(general_1, parser),
    LexTokens=element(2,Lexer),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    ?assertThrow({error,{format_mismatch,localtime}},iso8601:parse_time(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_localtime_test() ->
    Input   = iso8601_phases:get_localtime(general_1, input),
    Lexer   = iso8601_phases:get_localtime(general_1, lexer),
    Parser  = iso8601_phases:get_localtime(general_1, parser),
    Value   = iso8601_phases:get_localtime(general_1, value),
    TFormat  = iso8601_phases:get_localtime(general_1, format),
    TZFormat  = minute,
    LexTokens=element(2,Lexer),
    Tokens  = element(2,element(2,element(2,Parser))),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    meck:expect(iso8601,apply_time_tokens,
                fun(_Time,_UTime,_Tokens) ->
                        ?assertEqual(_Time,{0,1,1}),
                        ?assertEqual(_UTime,0),
                        ?assertEqual(Tokens,_Tokens),Value end),
    meck:expect(iso8601,apply_timezone_tokens,
                fun(_Direction, _Tokens) ->
                        ?assertEqual(_Direction, element(2,element(3,element(2,Parser)))),
                        ?assertEqual(_Tokens, element(3,element(3,element(2,Parser)))),
                        Value end),
    ?assertEqual({Value,{TFormat,TZFormat}},iso8601:parse_localtime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(iso8601),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_localtime_wrongformat_test() ->
    Input   = iso8601_phases:get_time(general_1, input),
    Lexer   = iso8601_phases:get_time(general_1, lexer),
    Parser  = iso8601_phases:get_time(general_1, parser),
    LexTokens=element(2,Lexer),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    ?assertThrow({error,{format_mismatch,time}},iso8601:parse_localtime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.


apply_timezone_tokens_test_() ->
    [
        ?_assertEqual( 0, iso8601:apply_timezone_tokens(add,[{hour,0}]) ),
        ?_assertEqual( 0, iso8601:apply_timezone_tokens(sub,[{hour,0}]) ),
        ?_assertEqual( 3600, iso8601:apply_timezone_tokens(add,[{hour,1}]) ),
        ?_assertEqual( -3600, iso8601:apply_timezone_tokens(sub,[{hour,1}]) ),
        ?_assertEqual( 7200, iso8601:apply_timezone_tokens(add,[{hour,2}]) ),
        ?_assertEqual( -7200, iso8601:apply_timezone_tokens(sub,[{hour,2}]) ),
        ?_assertEqual( -7260, iso8601:apply_timezone_tokens(sub,[{hour,2},{minute,1}]) ),
        ?_assertEqual( 4500, iso8601:apply_timezone_tokens(add,[{hour,1},{minute,15}]) ),
        ?_assertEqual( -7200, iso8601:apply_timezone_tokens(sub,[{hour,2},{minute,0}]) ),
        ?_assertEqual( 7200, iso8601:apply_timezone_tokens(add,[{hour,2},{minute,0}]) )
    ].

parse_datetime_test() ->
    Input   = iso8601_phases:get_datetime(general_1, input),
    Lexer   = iso8601_phases:get_datetime(general_1, lexer),
    Parser  = iso8601_phases:get_datetime(general_1, parser),
    Value   = iso8601_phases:get_datetime(general_1, value),
    LexTokens=element(2,Lexer),
    DateTokens  = element(2,element(2,element(2,Parser))),
    TimeTokens  = element(2,element(3,element(2,Parser))),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    meck:expect(iso8601,apply_time_tokens,
                fun(_Time,_UTime,_Tokens) ->
                        ?assertEqual(_Time,{0,1,1}),
                        ?assertEqual(_UTime,0),
                        ?assertEqual(TimeTokens,_Tokens),
                        {element(2,element(2,Value)),
                        element(3,element(2,Value)) }
                        end),
    meck:expect(iso8601,apply_date_tokens,
                fun(_Date,_Tokens) ->
                        ?assertEqual(_Date,{0,1,1}),
                        ?assertEqual(DateTokens,_Tokens),
                        element(1,element(2,Value))
                end),
    ?assertEqual(Value,iso8601:parse_datetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(iso8601),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_datetime_wrongformat_test() ->
    Input   = iso8601_phases:get_localdatetime(general_1, input),
    Lexer   = iso8601_phases:get_localdatetime(general_1, lexer),
    Parser  = iso8601_phases:get_localdatetime(general_1, parser),
    LexTokens=element(2,Lexer),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    ?assertThrow({error,{format_mismatch,datetime_local}},iso8601:parse_datetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.

parse_localdatetime_test() ->
    Input   = iso8601_phases:get_localdatetime(general_1, input),
    Lexer   = iso8601_phases:get_localdatetime(general_1, lexer),
    Parser  = iso8601_phases:get_localdatetime(general_1, parser),
    Value   = iso8601_phases:get_localdatetime(general_1, value),
    LexTokens=element(2,Lexer),
    DateTokens  = element(2,element(2,element(2,Parser))),
    TimeTokens  = element(2,element(3,element(2,Parser))),
    TZTokens  = element(2,element(4,element(2,Parser))),
    TZDirection= element(3,element(4,element(2,Parser))),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    meck:expect(iso8601,apply_time_tokens,
                fun(_Time,_UTime,_Tokens) ->
                        ?assertEqual(_Time,{0,1,1}),
                        ?assertEqual(_UTime,0),
                        ?assertEqual(TimeTokens,_Tokens),
                        {element(2,element(2,Value)),
                        element(3,element(2,Value)) }
                        end),
    meck:expect(iso8601,apply_date_tokens,
                fun(_Date,_Tokens) ->
                        ?assertEqual(_Date,{0,1,1}),
                        ?assertEqual(DateTokens,_Tokens),
                        element(1,element(2,Value))
                end),
    meck:expect(iso8601,apply_timezone_tokens,
                fun(_Direction, _Tokens) ->
                        ?assertEqual(_Direction, TZDirection),
                        ?assertEqual(_Tokens, TZTokens),
                        Value end),
    ?assertEqual(Value,iso8601:parse_localdatetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(iso8601),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.
parse_localdatetime_wrongformat_test() ->
    Input   = iso8601_phases:get_datetime(general_1, input),
    Lexer   = iso8601_phases:get_datetime(general_1, lexer),
    Parser  = iso8601_phases:get_datetime(general_1, parser),
    LexTokens=element(2,Lexer),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    ?assertThrow({error,{format_mismatch,datetime}},iso8601:parse_localdatetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    meck:unload(iso8601_lexer),
    meck:unload(iso8601_parser),
    ok.
