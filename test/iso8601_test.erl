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

parse_date_test_() ->
	[
		% Standard checks

		% Format checks
		?_assertEqual({1,1,1}, iso8601:parse_date(<<"00">>) ),
		?_assertEqual({1,1,1}, iso8601:parse_date("00") ),
		?_assertEqual({0,1,1}, iso8601:parse_date("0000") ),
		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_date("0000-01-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_date("00000101") ),

		?_assertEqual({0,1,1}, iso8601:parse_date("0000001") ),
		?_assertEqual({0,1,1}, iso8601:parse_date("0000-001") ),

		?_assertEqual({2001,1,1}, iso8601:parse_date("20") ),
		?_assertEqual({2013,1,1}, iso8601:parse_date("2013") ),
		?_assertEqual({2013,7,1}, iso8601:parse_date("2013-07") ),
		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-07-21") ),
		?_assertEqual({2013,7,21}, iso8601:parse_date("20130721") ),
		?_assertEqual({2013,7,21}, iso8601:parse_date("2013202") ),
		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-202") ),

		?_assertEqual({2013,1,1}, iso8601:parse_date("2013001") ),
		?_assertEqual({2013,1,1}, iso8601:parse_date("2013-001") ),

		?_assertEqual({2013,7,21}, iso8601:parse_date("2013W297") ),
		?_assertEqual({2013,7,21}, iso8601:parse_date("2013-W29-7") ),

		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W021") ),
		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02-1") ),

		?_assertEqual({2013,7,15}, iso8601:parse_date("2013W29") ),
		?_assertEqual({2013,7,15}, iso8601:parse_date("2013-W29") ),

		?_assertEqual({2013,1,7}, iso8601:parse_date("2013W02") ),
		?_assertEqual({2013,1,7}, iso8601:parse_date("2013-W02") ),

		% Binary format pasing
		?_assertEqual({2013,7,21}, iso8601:parse_date(<<"2013-07-21">>) ),

		% Wrong format checks
		?_assertThrow({error, {wrong_format,""}}, iso8601:parse_date("") ),
		?_assertThrow({error, {wrong_format,"poc"}}, iso8601:parse_date("poc") ),
		?_assertThrow({error, {wrong_format,"2013X01X01"}}, iso8601:parse_date("2013X01X01") ),


		% Standard day/month too_big/too_low checks
		?_assertThrow({error, {day_too_low,{2013,1,0}}}, iso8601:parse_date("2013-01-00") ),
		?_assertThrow({error, {day_too_low,{2013,2,0}}}, iso8601:parse_date("2013-02-00") ),
		?_assertThrow({error, {day_too_low,{2013,3,0}}}, iso8601:parse_date("2013-03-00") ),
		?_assertThrow({error, {day_too_low,{2013,4,0}}}, iso8601:parse_date("2013-04-00") ),
		?_assertThrow({error, {day_too_low,{2013,5,0}}}, iso8601:parse_date("2013-05-00") ),
		?_assertThrow({error, {day_too_low,{2013,6,0}}}, iso8601:parse_date("2013-06-00") ),
		?_assertThrow({error, {day_too_low,{2013,7,0}}}, iso8601:parse_date("2013-07-00") ),
		?_assertThrow({error, {day_too_low,{2013,8,0}}}, iso8601:parse_date("2013-08-00") ),
		?_assertThrow({error, {day_too_low,{2013,9,0}}}, iso8601:parse_date("2013-09-00") ),
		?_assertThrow({error, {day_too_low,{2013,10,0}}}, iso8601:parse_date("2013-10-00") ),
		?_assertThrow({error, {day_too_low,{2013,11,0}}}, iso8601:parse_date("2013-11-00") ),
		?_assertThrow({error, {day_too_low,{2013,12,0}}}, iso8601:parse_date("2013-12-00") ),
		?_assertThrow({error, {month_too_big,{2013,13,0}}}, iso8601:parse_date("2013-13-00") ),
		?_assertThrow({error, {month_too_low,{2013,0,1}}}, iso8601:parse_date("2013-00-01") ),

		?_assertThrow({error, {day_too_small,{2011,0}}}, iso8601:parse_date("2011-000") ),
		?_assertThrow({error, {day_too_big,{2011,600}}}, iso8601:parse_date("2011-600") ),

		?_assertEqual({2013,1,1}, iso8601:parse_date("2013-01-01") ),
		?_assertEqual({2013,2,1}, iso8601:parse_date("2013-02-01") ),
		?_assertEqual({2013,3,1}, iso8601:parse_date("2013-03-01") ),
		?_assertEqual({2013,4,1}, iso8601:parse_date("2013-04-01") ),
		?_assertEqual({2013,5,1}, iso8601:parse_date("2013-05-01") ),
		?_assertEqual({2013,6,1}, iso8601:parse_date("2013-06-01") ),
		?_assertEqual({2013,7,1}, iso8601:parse_date("2013-07-01") ),
		?_assertEqual({2013,8,1}, iso8601:parse_date("2013-08-01") ),
		?_assertEqual({2013,9,1}, iso8601:parse_date("2013-09-01") ),
		?_assertEqual({2013,10,1}, iso8601:parse_date("2013-10-01") ),
		?_assertEqual({2013,11,1}, iso8601:parse_date("2013-11-01") ),
		?_assertEqual({2013,12,1}, iso8601:parse_date("2013-12-01") ),
		?_assertThrow({error, {month_too_big,{2013,13,1}}}, iso8601:parse_date("2013-13-01") ),

		?_assertEqual({2013,1,31}, iso8601:parse_date("2013-01-31") ),
		?_assertEqual({2013,2,28}, iso8601:parse_date("2013-02-28") ),
		?_assertEqual({2013,3,31}, iso8601:parse_date("2013-03-31") ),
		?_assertEqual({2013,4,30}, iso8601:parse_date("2013-04-30") ),
		?_assertEqual({2013,5,31}, iso8601:parse_date("2013-05-31") ),
		?_assertEqual({2013,6,30}, iso8601:parse_date("2013-06-30") ),
		?_assertEqual({2013,7,31}, iso8601:parse_date("2013-07-31") ),
		?_assertEqual({2013,8,31}, iso8601:parse_date("2013-08-31") ),
		?_assertEqual({2013,9,30}, iso8601:parse_date("2013-09-30") ),
		?_assertEqual({2013,10,31}, iso8601:parse_date("2013-10-31") ),
		?_assertEqual({2013,11,30}, iso8601:parse_date("2013-11-30") ),
		?_assertEqual({2013,12,31}, iso8601:parse_date("2013-12-31") ),
		?_assertThrow({error, {month_too_big,{2013,13,30}}}, iso8601:parse_date("2013-13-30") ),


		?_assertThrow({error, {day_too_big,{2013,01,32}}}, iso8601:parse_date("2013-01-32") ),
		?_assertThrow({error, {day_too_big,{2013,02,29}}}, iso8601:parse_date("2013-02-29") ),
		?_assertThrow({error, {day_too_big,{2013,03,32}}}, iso8601:parse_date("2013-03-32") ),
		?_assertThrow({error, {day_too_big,{2013,04,31}}}, iso8601:parse_date("2013-04-31") ),
		?_assertThrow({error, {day_too_big,{2013,05,32}}}, iso8601:parse_date("2013-05-32") ),
		?_assertThrow({error, {day_too_big,{2013,06,31}}}, iso8601:parse_date("2013-06-31") ),
		?_assertThrow({error, {day_too_big,{2013,07,32}}}, iso8601:parse_date("2013-07-32") ),
		?_assertThrow({error, {day_too_big,{2013,08,32}}}, iso8601:parse_date("2013-08-32") ),
		?_assertThrow({error, {day_too_big,{2013,09,31}}}, iso8601:parse_date("2013-09-31") ),
		?_assertThrow({error, {day_too_big,{2013,10,32}}}, iso8601:parse_date("2013-10-32") ),
		?_assertThrow({error, {day_too_big,{2013,11,31}}}, iso8601:parse_date("2013-11-31") ),
		?_assertThrow({error, {day_too_big,{2013,12,32}}}, iso8601:parse_date("2013-12-32") ),
		?_assertThrow({error, {month_too_big,{2013,13,32}}}, iso8601:parse_date("2013-13-32") ),

		% Week day tests
		?_assertThrow({error, {week_too_small,{2011,0}}}, iso8601:parse_date("2011-W00-1") ),
		?_assertThrow({error, {week_too_big,{2011,99}}}, iso8601:parse_date("2011-W99-1") ),
		?_assertEqual({2009,12,28}, iso8601:parse_date("2009-W53-1") ),
		?_assertEqual({2010,12,27}, iso8601:parse_date("2010-W52-1") ),
		?_assertEqual({2011,12,26}, iso8601:parse_date("2011-W52-1") ),
		?_assertEqual({2012,1,1}, iso8601:parse_date("2011-W52-7") ), % Skipping to next year
		?_assertEqual({2012,12,24}, iso8601:parse_date("2012-W52-1") ),
		?_assertEqual({2013,12,23}, iso8601:parse_date("2013-W52-1") ),
		?_assertEqual({2012,12,24}, iso8601:parse_date("2012-W52") ),
		?_assertEqual({2013,12,23}, iso8601:parse_date("2013-W52") ),
		?_assertEqual({2012,12,24}, iso8601:parse_date("2012W52") ),
		?_assertEqual({2013,12,23}, iso8601:parse_date("2013W52") ),
		?_assertThrow({error, {week_too_big,{2009,54}}}, iso8601:parse_date("2009-W54-1") ),
		?_assertThrow({error, {week_too_big,{2012,53}}}, iso8601:parse_date("2012-W53-1") ),
		?_assertThrow({error, {week_too_big,{2013,53}}}, iso8601:parse_date("2013-W53-1") ),

		?_assertThrow({error, {wrong_format,"2011-W11-0"}}, iso8601:parse_date("2011-W11-0") ),
		?_assertThrow({error, {wrong_format,"2011-W11-8"}}, iso8601:parse_date("2011-W11-8") ),

		% leep day check
		?_assertThrow({error, {day_too_low,{2012,2,0}}}, iso8601:parse_date("2012-02-00") ),
		?_assertEqual({2012,2,1}, iso8601:parse_date("2012-02-01") ),
		?_assertEqual({2012,2,29}, iso8601:parse_date("2012-02-29") ),
		?_assertThrow({error, {day_too_big,{2012,02,30}}}, iso8601:parse_date("2012-02-30") ),
		?_assertEqual({2012,12,31}, iso8601:parse_date("2012-366") ),
		?_assertThrow({error, {day_too_big,{2012,367}}}, iso8601:parse_date("2012-367") ),

		?_assertThrow({error, {day_too_low,{2011,2,0}}}, iso8601:parse_date("2011-02-00") ),
		?_assertEqual({2011,2,1}, iso8601:parse_date("2011-02-01") ),
		?_assertEqual({2011,2,28}, iso8601:parse_date("2011-02-28") ),
		?_assertThrow({error, {day_too_big,{2011,02,29}}}, iso8601:parse_date("2011-02-29") ),
		?_assertEqual({2011,12,31}, iso8601:parse_date("2011-365") ),
		?_assertThrow({error, {day_too_big,{2011,366}}}, iso8601:parse_date("2011-366") )
	].

format_date_test_() ->
	[
		?_assertEqual("2011-12-31", iso8601:format_date({2011,12,31}) ),
		?_assertEqual("20111231", iso8601:format_date({2011,12,31}, calendar) ),
		?_assertEqual("2011-12-31", iso8601:format_date({2011,12,31}, calendar_extended) ),
		?_assertEqual("2011-12", iso8601:format_date({2011,12,31}, calendar_month) ),
		?_assertEqual("2011", iso8601:format_date({2011,12,31}, calendar_year) ),
		?_assertEqual("20", iso8601:format_date({2011,12,31}, calendar_century) ),
		?_assertEqual("2011365", iso8601:format_date({2011,12,31}, ordinal) ),
		?_assertEqual("2011-365", iso8601:format_date({2011,12,31}, ordinal_extended) ),
		?_assertEqual("2011W526", iso8601:format_date({2011,12,31}, weekday) ),
		?_assertEqual("2011-W52-6", iso8601:format_date({2011,12,31}, weekday_extended) ),
		?_assertEqual("2011W52", iso8601:format_date({2011,12,31}, week) ),
		?_assertEqual("2011-W52", iso8601:format_date({2011,12,31}, week_extended) ),


		?_assertEqual("0011-01-02", iso8601:format_date({11,1,2}) ),
		?_assertEqual("00110102", iso8601:format_date({11,1,2}, calendar) ),
		?_assertEqual("0011-01-02", iso8601:format_date({11,1,2}, calendar_extended) ),
		?_assertEqual("0011-01", iso8601:format_date({11,1,31}, calendar_month) ),
		?_assertEqual("0011", iso8601:format_date({11,1,31}, calendar_year) ),
		?_assertEqual("20", iso8601:format_date({2011,1,31}, calendar_century) ),
		?_assertEqual("2011031", iso8601:format_date({2011,1,31}, ordinal) ),
		?_assertEqual("2011-031", iso8601:format_date({2011,1,31}, ordinal_extended) ),
		?_assertEqual("2011W011", iso8601:format_date({2011,1,3}, weekday) ),
		?_assertEqual("2011-W01-1", iso8601:format_date({2011,1,3}, weekday_extended) ),
		?_assertEqual("2011W01", iso8601:format_date({2011,1,3}, week) ),
		?_assertEqual("2011-W01", iso8601:format_date({2011,1,3}, week_extended) ),
		?_assertThrow({error, {unknown_format,wtf}},iso8601:format_date({2011,1,3}, wtf)),
		?_assertThrow({error, {unknown_data,wtf}},iso8601:format_date(wtf, calendar)),
		?_assertThrow({error, {unknown_data,wtf}},iso8601:format_date(wtf, wtf2))

	].
