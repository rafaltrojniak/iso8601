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

%% Date tokens tests
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

apply_date_tokens_edges_test_() ->
    [
        ?_assertThrow( {error, {yearday_too_low,{0,0}}}, iso8601:apply_date_tokens({0,1,1},[{yearday,0}])),
        ?_assertEqual( {0,1,1}, iso8601:apply_date_tokens({0,1,1},[{yearday,1}])),
        ?_assertEqual( {0,12,31}, iso8601:apply_date_tokens({0,1,1},[{yearday,366}])),
        ?_assertThrow( {error, {yearday_too_big,{0,367}}}, iso8601:apply_date_tokens({0,1,1},[{yearday,367}])),

        ?_assertThrow( {error, {weeknumber_too_low,0}}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,0}])),
        ?_assertEqual( {0,1,3}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,1}])),
        % Weeks count in year tests
        ?_assertEqual( {2009,12,28}, iso8601:apply_date_tokens({2009,1,1},[{weeknumber,53}])),
        ?_assertThrow( {error, {weeknumber_too_big,{2009,54}}}, iso8601:apply_date_tokens({2009,1,1},[{weeknumber,54}])),
        ?_assertEqual( {2013,12,23}, iso8601:apply_date_tokens({2013,1,1},[{weeknumber,52}])),
        ?_assertThrow( {error, {weeknumber_too_big,{2013,53}}}, iso8601:apply_date_tokens({2013,1,1},[{weeknumber,53}])),
        % Week shift on beginning/end
        ?_assertEqual( {2012,1,1}, iso8601:apply_date_tokens({2011,1,1},[{weeknumber,52},{weekday,7}])),
        ?_assertEqual( {2012,12,31}, iso8601:apply_date_tokens({2013,1,1},[{weeknumber,1},{weekday,1}])),

        ?_assertThrow( {error, {weekday_too_low,0}}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,1},{weekday,0}])),
        ?_assertEqual( {0,1,3}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,1},{weekday,1}])),
        ?_assertEqual( {0,1,9}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,1},{weekday,7}])),
        ?_assertThrow( {error, {weekday_too_big,8}}, iso8601:apply_date_tokens({0,1,1},[{weeknumber,1},{weekday,8}])),

        ?_assertEqual( {1,12,31}, iso8601:apply_date_tokens({1,1,1},[{yearday,365}])),
        ?_assertThrow( {error, {yearday_too_big,{1,366}}}, iso8601:apply_date_tokens({1,1,1},[{yearday,366}])),

        ?_assertThrow( {error, {month_too_low,0}}, iso8601:apply_date_tokens({0,1,1},[{month,0}])),
        ?_assertEqual( {0,1,1}, iso8601:apply_date_tokens({0,1,1},[{month,1}])),
        ?_assertEqual( {0,12,1}, iso8601:apply_date_tokens({0,1,1},[{month,12}])),
        ?_assertThrow( {error, {month_too_big,13}}, iso8601:apply_date_tokens({0,1,1},[{month,13}])),
        ?_assertThrow( {error, {day_too_low,0}}, iso8601:apply_date_tokens({0,1,1},[{monthday,0}])),
        ?_assertEqual( {0,1,1}, iso8601:apply_date_tokens({0,1,1},[{monthday,1}])),
        ?_assertEqual( {0,1,31}, iso8601:apply_date_tokens({0,1,1},[{monthday,31}])),
        ?_assertThrow( {error, {day_too_big,{0,1,32}}}, iso8601:apply_date_tokens({0,1,1},[{monthday,32}])),

        ?_assertThrow( {unknown_token, {unknownToken,32}}, iso8601:apply_date_tokens({0,1,1},[{unknownToken,32}]))
    ].

%% Date parsing tests

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
    meck:unload(),
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
    meck:unload(),
    ok.

%% Formatting date

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

format_date_edges_test_() ->
    [
            ?_assertThrow({error,{unknown_format,wrongFormat}}, iso8601:format_date({1,1,1}, wrongFormat)),
            ?_assertThrow({error,{unknown_data,{1,1,"abc"}}}, iso8601:format_date({1,1,"abc"}, calendar))
    ].

format_date_default_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_date,
                fun(Date,Format) ->
                        ?assertEqual({2013,1,1},Date),
                        ?assertEqual(calendar,Format),
                        "20130101"
                end),
    ?assertEqual("20130101",iso8601:format_date({2013,1,1})),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

%% Time tokens tests

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


apply_time_tokens_edges_test_() ->
    [
        ?_assertEqual( {{2,24,0},0}, iso8601:apply_time_tokens({0,0,0},0,[{frac,3600*24,"1"}])),
        ?_assertEqual( {{0,6,0},0}, iso8601:apply_time_tokens({0,0,0},0,[{frac,3600,"1"}])),
        ?_assertEqual( {{0,0,6},0}, iso8601:apply_time_tokens({0,0,0},0,[{frac,60,"1"}])),
        ?_assertEqual( {{0,0,0},999999}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"999999"}])),
        ?_assertEqual( {{0,0,0},900000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"9"}])),
        ?_assertEqual( {{0,0,0},90000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"09"}])),
        ?_assertEqual( {{0,0,0},9000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"009"}])),
        ?_assertEqual( {{0,0,0},900}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0009"}])),
        ?_assertEqual( {{0,0,0},90}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"00009"}])),
        ?_assertEqual( {{0,0,0},100000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"1"}])),
        ?_assertEqual( {{0,0,0},10000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"01"}])),
        ?_assertEqual( {{0,0,0},1000}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"001"}])),
        ?_assertEqual( {{0,0,0},100}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0001"}])),
        ?_assertEqual( {{0,0,0},10}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"00001"}])),
        ?_assertEqual( {{0,0,0},1}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"000001"}])),
        ?_assertEqual( {{0,0,0},0}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0000001"}])),
        ?_assertEqual( {{0,0,0},0}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0000004"}])),
        ?_assertEqual( {{0,0,0},1}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0000005"}])),
        ?_assertEqual( {{0,0,0},1}, iso8601:apply_time_tokens({0,0,0},0,[{frac,1,"0000006"}])),
        ?_assertThrow( {unknown_token, {unknownToken,32}}, iso8601:apply_time_tokens({0,0,0},0,[{unknownToken,32}]))
    ].

%% Parsing time tests

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
                        ?assertEqual({0,0,0},_Time),
                        ?assertEqual(0,_UTime),
                        ?assertEqual(Tokens,_Tokens),Value end),
    ?assertEqual({Value,Format},iso8601:parse_time(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
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
    meck:unload(),
    ok.

% Time genration
format_time_defualt_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        ?assertEqual(general,Format),
                        "T100502"
                end),
    ?assertEqual("T100502",iso8601:format_time({10,5,2},0)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

format_time_defualt_utime1_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(10,Utime),
                        ?assertEqual({general_frac,6},Format),
                        "T100502,000010"
                end),
    ?assertEqual("T100502,000010",iso8601:format_time({10,5,2},10)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

format_time_defualt_utime2_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(100,Utime),
                        ?assertEqual({general_frac,6},Format),
                        "T100502,000100"
                end),
    ?assertEqual("T100502,000100",iso8601:format_time({10,5,2},100)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

format_time_test_() ->
    [
        ?_assertEqual( "T010101", iso8601:format_time({1,1,1},0,general)),
        ?_assertEqual( "T101010", iso8601:format_time({10,10,10},0,general)),
        ?_assertEqual( "T01:01:01", iso8601:format_time({1,1,1},0,general_extended)),
        ?_assertEqual( "T0101", iso8601:format_time({1,1,1},0,general_minute)),
        ?_assertEqual( "T01:01", iso8601:format_time({1,1,1},0,general_minute_extended)),
        ?_assertEqual( "T01", iso8601:format_time({1,1,1},0,general_hour)),

        ?_assertEqual( "T010101,123", iso8601:format_time({1,1,1},123456,{general_frac,3})),
        ?_assertEqual( "T101010,123", iso8601:format_time({10,10,10},123456,{general_frac,3})),
        ?_assertEqual( "T01:01:01,123", iso8601:format_time({1,1,1},123456,{general_extended_frac,3})),
        ?_assertEqual( "T0101,019", iso8601:format_time({1,1,1},123456,{general_minute_frac,3})),
        ?_assertEqual( "T01:01,019", iso8601:format_time({1,1,1},123456,{general_minute_extended_frac,3})),
        ?_assertEqual( "T01,017", iso8601:format_time({1,1,1},123456,{general_hour_frac,3})),
        ?_assertEqual( "T01,02", iso8601:format_time({1,1,1},123456,{general_hour_frac,2})),
        ?_assertEqual( "T01,0", iso8601:format_time({1,1,1},123456,{general_hour_frac,1})),

        ?_assertThrow( {error,{unknown_format,wtf}}, iso8601:format_time({1,1,1},0,wtf))
    ].

%% Applyting timezone tokens

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

%% Parsing localtime
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
                        ?assertEqual({0,0,0},_Time),
                        ?assertEqual(0,_UTime),
                        ?assertEqual(Tokens,_Tokens),
                        { element(1,Value),
                        element(2,Value)}
                end),
    meck:expect(iso8601,apply_timezone_tokens,
                fun(_Direction, _Tokens) ->
                        ?assertEqual(_Direction, element(2,element(3,element(2,Parser)))),
                        ?assertEqual(_Tokens, element(3,element(3,element(2,Parser)))),
                        element(3,Value) end),
    ?assertEqual({Value,{TFormat,TZFormat}},iso8601:parse_localtime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
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
    meck:unload(),
    ok.


%% Parsing datetime
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
                        ?assertEqual({0,0,0},_Time),
                        ?assertEqual(0,_UTime),
                        ?assertEqual(TimeTokens,_Tokens),
                        {element(2,element(1,Value)),
                        element(3,element(1,Value)) }
                        end),
    meck:expect(iso8601,apply_date_tokens,
                fun(_Date,_Tokens) ->
                        ?assertEqual({0,1,1},_Date),
                        ?assertEqual(DateTokens,_Tokens),
                        element(1,element(1,Value))
                end),
    ?assertEqual(Value,iso8601:parse_datetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
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
    meck:unload(),
    ok.

%% datetime generation
gen_datetime_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_date,
                fun(Date,Format) ->
                        ?assertEqual({2013,1,1},Date),
                        ?assertEqual(calendar,Format),
                        "20130101"
                end),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        ?assertEqual(general,Format),
                        "T100502"
                end),
    ?assertEqual("20130101T100502",iso8601:format_datetime({2013,1,1},{10,5,2},0,calendar,general)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.


format_datetime_defualt_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_date,
                fun(Date) ->
                        ?assertEqual({2013,1,1},Date),
                        "20130101"
                end),
    meck:expect(iso8601,format_time,
                fun(Time,Utime) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        "T100502"
                end),
    ?assertEqual("20130101T100502",iso8601:format_datetime({2013,1,1},{10,5,2},0)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

%% Parsing local datetime
parse_localdatetime_test() ->
    Input   = iso8601_phases:get_localdatetime(general_1, input),
    Lexer   = iso8601_phases:get_localdatetime(general_1, lexer),
    Parser  = iso8601_phases:get_localdatetime(general_1, parser),
    Value   = iso8601_phases:get_localdatetime(general_1, value),
    LexTokens=element(2,Lexer),
    DateTokens  = element(2,element(2,element(2,Parser))),
    TimeTokens  = element(2,element(3,element(2,Parser))),
    TZDirection= element(2,element(4,element(2,Parser))),
    TZTokens  = element(3,element(4,element(2,Parser))),
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Lexer) -> ?assertEqual(LexTokens,_Lexer), Parser end),
    meck:expect(iso8601,apply_time_tokens,
                fun(_Time,_UTime,_Tokens) ->
                        ?assertEqual({0,0,0},_Time),
                        ?assertEqual(0,_UTime),
                        ?assertEqual(TimeTokens,_Tokens),
                        {element(2,element(1,Value)),
                        element(3,element(1,Value)) }
                        end),
    meck:expect(iso8601,apply_date_tokens,
                fun(_Date,_Tokens) ->
                        ?assertEqual({0,1,1},_Date),
                        ?assertEqual(DateTokens,_Tokens),
                        element(1,element(1,Value))
                end),
    meck:expect(iso8601,apply_timezone_tokens,
                fun(_Direction, _Tokens) ->
                        ?assertEqual(TZDirection,_Direction),
                        ?assertEqual(_Tokens, TZTokens),
                        element(4,element(1,Value))end),
    ?assertEqual(Value,iso8601:parse_localdatetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
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
    meck:unload(),
    ok.

% Lexer/Parser fails tests
parse_localdatetime_lexfail_test() ->
    Input="poc",
    Lexer={error, {1,iso8601_lexer,{illegal,"p"}} ,1 },
    meck:new(iso8601_lexer),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    ?assertThrow({error, {lexical_analysis_failed,_,1}},iso8601:parse_localdatetime(Input)),
    ?assert(meck:validate(iso8601_lexer)),
    meck:unload(),
    ok.

parse_localdatetime_parserfail_test() ->
    Input=":::",
    Lexer={ok,[ {time_separator,":",":"},{time_separator,":",":"}, {time_separator,":",":"}],1},
    LexerTokens=element(2,Lexer),
    Parser= {error,{":",iso8601_parser, ["syntax error before: ",["\":\""]]}},
    meck:new(iso8601_lexer),
    meck:new(iso8601_parser),
    meck:expect(iso8601_lexer,string,fun(_Input) -> ?assertEqual(Input,_Input), Lexer end),
    meck:expect(iso8601_parser,parse,fun(_Tokens) -> ?assertEqual(LexerTokens,_Tokens), Parser end),
    ?assertThrow({error, {parsing_failed,_}},iso8601:parse_localdatetime(Input)),
    ?assert(meck:validate(iso8601_parser)),
    ?assert(meck:validate(iso8601_lexer)),
    meck:unload(),
    ok.

%% Local datetime generation
gen_localdatetime_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_date,
                fun(Date,Format) ->
                        ?assertEqual({2013,1,1},Date),
                        ?assertEqual(calendar,Format),
                        "20130101"
                end),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        ?assertEqual(general,Format),
                        "T100502"
                end),
    meck:expect(iso8601,format_timezone,
                fun(TZ,Format) ->
                        ?assertEqual(36000,TZ),
                        ?assertEqual(minute,Format),
                        "+1000"
                end),
    ?assertEqual("20130101T100502+1000",iso8601:format_localdatetime({2013,1,1},{10,5,2},0,36000,calendar,general,minute)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

%% local datetime generation
gen_localdatetime_default_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_date,
                fun(Date) ->
                        ?assertEqual({2013,1,1},Date),
                        "20130101"
                end),
    meck:expect(iso8601,format_time,
                fun(Time,Utime) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        "T100502"
                end),
    meck:expect(iso8601,format_timezone,
                fun(TZ) ->
                        ?assertEqual(36000,TZ),
                        "+1000"
                end),
    ?assertEqual("20130101T100502+1000",iso8601:format_localdatetime({2013,1,1},{10,5,2},0,36000)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.


%% Formatting timezone
format_timezone_test_() ->
    [
        ?_assertEqual( "+0000", iso8601:format_timezone(0,minute)),
        ?_assertEqual( "-0100", iso8601:format_timezone(-3600,minute)),
        ?_assertEqual( "-1000", iso8601:format_timezone(-36000,minute)),
        ?_assertEqual( "+1000", iso8601:format_timezone(36000,minute)),
        ?_assertEqual( "+00:00", iso8601:format_timezone(0,minute_extended)),
        ?_assertEqual( "+01:00", iso8601:format_timezone(3600,minute_extended)),
        ?_assertEqual( "+10:00", iso8601:format_timezone(3600*10,minute_extended)),
        ?_assertEqual( "-10:00", iso8601:format_timezone(-3600*10,minute_extended)),
        ?_assertEqual( "+10:01", iso8601:format_timezone(3600*10+60,minute_extended)),
        ?_assertEqual( "+10:30", iso8601:format_timezone(3600*10+60*30,minute_extended)),
        ?_assertEqual( "+00", iso8601:format_timezone(0,hour)),
        ?_assertEqual( "+01", iso8601:format_timezone(3600,hour)),
        ?_assertEqual( "+10", iso8601:format_timezone(3600*10,hour)),
        ?_assertEqual( "-10", iso8601:format_timezone(-3600*10,hour)),
        ?_assertEqual( "Z", iso8601:format_timezone(0,utc)),
        ?_assertThrow( {error,{unknown_format,wtf}}, iso8601:format_timezone(0,wtf))
    ].
gen_timezone_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_timezone,
                fun(TZ,Format) ->
                        ?assertEqual(36000,TZ),
                        ?assertEqual(minute,Format),
                        "+1000"
                end),
    ?assertEqual("+1000",iso8601:format_timezone(36000)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

%% Localtime generation
gen_localtime_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_time,
                fun(Time,Utime,Format) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        ?assertEqual(general,Format),
                        "T100502"
                end),
    meck:expect(iso8601,format_timezone,
                fun(TZ,Format) ->
                        ?assertEqual(36000,TZ),
                        ?assertEqual(minute,Format),
                        "+1000"
                end),
    ?assertEqual("T100502+1000",iso8601:format_localtime({10,5,2},0,36000,general,minute)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

gen_localtime_default_test() ->
    meck:new(iso8601,[passthrough]),
    meck:expect(iso8601,format_time,
                fun(Time,Utime) ->
                        ?assertEqual({10,5,2},Time),
                        ?assertEqual(0,Utime),
                        "T100502"
                end),
    meck:expect(iso8601,format_timezone,
                fun(TZ) ->
                        ?assertEqual(36000,TZ),
                        "+1000"
                end),
    ?assertEqual("T100502+1000",iso8601:format_localtime({10,5,2},0,36000)),
    ?assert(meck:validate(iso8601)),
    meck:unload(),
    ok.

