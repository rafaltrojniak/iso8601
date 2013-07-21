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

parse_iso8601_date_calendar_test_() ->
	[
		% Standard checks

		% Format checks
		?_assertEqual({1,1,1}, iso8601:parse_iso8601_date_calendar(<<"+000">>) ),
		?_assertEqual({1,1,1}, iso8601:parse_iso8601_date_calendar("00") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("0000") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("0000-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("0000-01-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("00000101") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("+00000-01-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("+00000-01") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("+00000") ),
		?_assertEqual({1,1,1}, iso8601:parse_iso8601_date_calendar("+000") ),
		?_assertEqual({0,1,1}, iso8601:parse_iso8601_date_calendar("+000000101") ),

		?_assertEqual({2001,1,1}, iso8601:parse_iso8601_date_calendar("+020") ),
		?_assertEqual({2001,1,1}, iso8601:parse_iso8601_date_calendar("20") ),
		?_assertEqual({2013,1,1}, iso8601:parse_iso8601_date_calendar("2013") ),
		?_assertEqual({2013,7,1}, iso8601:parse_iso8601_date_calendar("2013-07") ),
		?_assertEqual({2013,7,21}, iso8601:parse_iso8601_date_calendar("2013-07-21") ),
		?_assertEqual({2013,7,21}, iso8601:parse_iso8601_date_calendar("20130721") ),
		?_assertEqual({2013,7,21}, iso8601:parse_iso8601_date_calendar("+02013-07-21") ),
		?_assertEqual({2013,7,1}, iso8601:parse_iso8601_date_calendar("+02013-07") ),
		?_assertEqual({2013,1,1}, iso8601:parse_iso8601_date_calendar("+02013") ),
		?_assertEqual({2001,1,1}, iso8601:parse_iso8601_date_calendar("+020") ),
		?_assertEqual({2013,7,21}, iso8601:parse_iso8601_date_calendar("+020130721") ),

		% Binary format pasing
		?_assertEqual({2013,7,21}, iso8601:parse_iso8601_date_calendar(<<"+02013-07-21">>) ),

		% Wrong format checks
		?_assertThrow({failed_too_parse,wrong_format,""}, iso8601:parse_iso8601_date_calendar("") ),
		?_assertThrow({failed_too_parse,wrong_format,"poc"}, iso8601:parse_iso8601_date_calendar("poc") ),
		?_assertThrow({failed_too_parse,wrong_format,"2013W13"}, iso8601:parse_iso8601_date_calendar("2013W13") ),
		?_assertThrow({failed_too_parse,wrong_format,"2013X01X01"}, iso8601:parse_iso8601_date_calendar("2013X01X01") ),


		% Standard day/month too_big/too_low checks
		?_assertThrow({failed_too_parse,day_too_low,{2013,1,0}}, iso8601:parse_iso8601_date_calendar("2013-01-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,2,0}}, iso8601:parse_iso8601_date_calendar("2013-02-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,3,0}}, iso8601:parse_iso8601_date_calendar("2013-03-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,4,0}}, iso8601:parse_iso8601_date_calendar("2013-04-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,5,0}}, iso8601:parse_iso8601_date_calendar("2013-05-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,6,0}}, iso8601:parse_iso8601_date_calendar("2013-06-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,7,0}}, iso8601:parse_iso8601_date_calendar("2013-07-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,8,0}}, iso8601:parse_iso8601_date_calendar("2013-08-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,9,0}}, iso8601:parse_iso8601_date_calendar("2013-09-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,10,0}}, iso8601:parse_iso8601_date_calendar("2013-10-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,11,0}}, iso8601:parse_iso8601_date_calendar("2013-11-00") ),
		?_assertThrow({failed_too_parse,day_too_low,{2013,12,0}}, iso8601:parse_iso8601_date_calendar("2013-12-00") ),
		?_assertThrow({failed_too_parse,month_too_big,{2013,13,0}}, iso8601:parse_iso8601_date_calendar("2013-13-00") ),

		?_assertEqual({2013,1,1}, iso8601:parse_iso8601_date_calendar("2013-01-01") ),
		?_assertEqual({2013,2,1}, iso8601:parse_iso8601_date_calendar("2013-02-01") ),
		?_assertEqual({2013,3,1}, iso8601:parse_iso8601_date_calendar("2013-03-01") ),
		?_assertEqual({2013,4,1}, iso8601:parse_iso8601_date_calendar("2013-04-01") ),
		?_assertEqual({2013,5,1}, iso8601:parse_iso8601_date_calendar("2013-05-01") ),
		?_assertEqual({2013,6,1}, iso8601:parse_iso8601_date_calendar("2013-06-01") ),
		?_assertEqual({2013,7,1}, iso8601:parse_iso8601_date_calendar("2013-07-01") ),
		?_assertEqual({2013,8,1}, iso8601:parse_iso8601_date_calendar("2013-08-01") ),
		?_assertEqual({2013,9,1}, iso8601:parse_iso8601_date_calendar("2013-09-01") ),
		?_assertEqual({2013,10,1}, iso8601:parse_iso8601_date_calendar("2013-10-01") ),
		?_assertEqual({2013,11,1}, iso8601:parse_iso8601_date_calendar("2013-11-01") ),
		?_assertEqual({2013,12,1}, iso8601:parse_iso8601_date_calendar("2013-12-01") ),
		?_assertThrow({failed_too_parse,month_too_big,{2013,13,1}}, iso8601:parse_iso8601_date_calendar("2013-13-01") ),

		?_assertEqual({2013,1,31}, iso8601:parse_iso8601_date_calendar("2013-01-31") ),
		?_assertEqual({2013,2,28}, iso8601:parse_iso8601_date_calendar("2013-02-28") ),
		?_assertEqual({2013,3,31}, iso8601:parse_iso8601_date_calendar("2013-03-31") ),
		?_assertEqual({2013,4,30}, iso8601:parse_iso8601_date_calendar("2013-04-30") ),
		?_assertEqual({2013,5,31}, iso8601:parse_iso8601_date_calendar("2013-05-31") ),
		?_assertEqual({2013,6,30}, iso8601:parse_iso8601_date_calendar("2013-06-30") ),
		?_assertEqual({2013,7,31}, iso8601:parse_iso8601_date_calendar("2013-07-31") ),
		?_assertEqual({2013,8,31}, iso8601:parse_iso8601_date_calendar("2013-08-31") ),
		?_assertEqual({2013,9,30}, iso8601:parse_iso8601_date_calendar("2013-09-30") ),
		?_assertEqual({2013,10,31}, iso8601:parse_iso8601_date_calendar("2013-10-31") ),
		?_assertEqual({2013,11,30}, iso8601:parse_iso8601_date_calendar("2013-11-30") ),
		?_assertEqual({2013,12,31}, iso8601:parse_iso8601_date_calendar("2013-12-31") ),
		?_assertThrow({failed_too_parse,month_too_big,{2013,13,30}}, iso8601:parse_iso8601_date_calendar("2013-13-30") ),


		?_assertThrow({failed_too_parse,day_too_big,{2013,01,32}}, iso8601:parse_iso8601_date_calendar("2013-01-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,02,29}}, iso8601:parse_iso8601_date_calendar("2013-02-29") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,03,32}}, iso8601:parse_iso8601_date_calendar("2013-03-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,04,31}}, iso8601:parse_iso8601_date_calendar("2013-04-31") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,05,32}}, iso8601:parse_iso8601_date_calendar("2013-05-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,06,31}}, iso8601:parse_iso8601_date_calendar("2013-06-31") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,07,32}}, iso8601:parse_iso8601_date_calendar("2013-07-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,08,32}}, iso8601:parse_iso8601_date_calendar("2013-08-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,09,31}}, iso8601:parse_iso8601_date_calendar("2013-09-31") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,10,32}}, iso8601:parse_iso8601_date_calendar("2013-10-32") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,11,31}}, iso8601:parse_iso8601_date_calendar("2013-11-31") ),
		?_assertThrow({failed_too_parse,day_too_big,{2013,12,32}}, iso8601:parse_iso8601_date_calendar("2013-12-32") ),
		?_assertThrow({failed_too_parse,month_too_big,{2013,13,32}}, iso8601:parse_iso8601_date_calendar("2013-13-32") ),

		% leep day check
		?_assertThrow({failed_too_parse,day_too_low,{2012,2,0}}, iso8601:parse_iso8601_date_calendar("2012-02-00") ),
		?_assertEqual({2012,2,1}, iso8601:parse_iso8601_date_calendar("2012-02-01") ),
		?_assertEqual({2012,2,29}, iso8601:parse_iso8601_date_calendar("2012-02-29") ),
		?_assertThrow({failed_too_parse,day_too_big,{2012,02,30}}, iso8601:parse_iso8601_date_calendar("2012-02-30") ),

		?_assertThrow({failed_too_parse,day_too_low,{2011,2,0}}, iso8601:parse_iso8601_date_calendar("2011-02-00") ),
		?_assertEqual({2011,2,1}, iso8601:parse_iso8601_date_calendar("2011-02-01") ),
		?_assertEqual({2011,2,28}, iso8601:parse_iso8601_date_calendar("2011-02-28") ),
		?_assertThrow({failed_too_parse,day_too_big,{2011,02,29}}, iso8601:parse_iso8601_date_calendar("2011-02-29") )
	].
