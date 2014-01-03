%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%   Phases of data between parocessing stages for unit tests
%%% @end
%%% Created :  czw gru 26 20:34:09 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601_phases).
%% API
-export([get_date/2, get_time/2, get_localtime/2, gen_stages/0]).

gen_stages()  ->
    lists:foreach(
  fun(X)  ->
    try
  LexerResult = iso8601_lexer:string(get_localtime(X,input)),
  ParseResult = iso8601_parser:parse(element(2,LexerResult )),
  io:format("get_localtime(~p, input)\t ->  \t~p;\n",[X,get_localtime(X,input)]),
  io:format("get_localtime(~p, format)\t ->  \t~p;\n",[X,get_localtime(X,format)]),
  io:format("get_localtime(~p, lexer)\t ->  \t~p;\n",[X,LexerResult]),
  io:format("get_localtime(~p, parser)\t ->  \t~p;\n",[X,ParseResult ])
    catch
  error:_ ->  ok;
  throw:_ ->  ok;
  exit:_  ->  ok
    end
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
    ]
     ).

-spec(get_date(Test::atom(), Stage::input|lexer|parser|value|format )  ->  any()).

% Calendar century
get_date(calendar_century_1, input) ->  "00";
get_date(calendar_century_1, lexer) ->  [{decimal,"0",0},{decimal,"0",0}];
get_date(calendar_century_1, parser)  ->  {date, {calendar_century,[{century,0}]}};
get_date(calendar_century_1, value) ->  {1,1,1};
get_date(calendar_century_1, format) ->  calendar_century;
get_date(calendar_century_2, input) ->  "20";
get_date(calendar_century_2, lexer) ->  [{decimal,"2",2},{decimal,"0",0}];
get_date(calendar_century_2, parser)  ->  {date, {calendar_century,[{century,20}]}};
get_date(calendar_century_2, value) ->  {2001,1,1};
get_date(calendar_century_2, format) ->  calendar_century;

% Calendar year
get_date(calendar_year_1, input)  ->  "0000";
get_date(calendar_year_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}];
get_date(calendar_year_1, parser) ->  {date,{calendar_year,[{year,0}]}};
get_date(calendar_year_1, value)  ->  {0,1,1};
get_date(calendar_year_1, format)  ->  calendar_year;
get_date(calendar_year_2, input)  ->  "2013";
get_date(calendar_year_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}];
get_date(calendar_year_2, parser) ->  {date, {calendar_year, [{year,2013}]}};
get_date(calendar_year_2, value)  ->  {2013,1,1};
get_date(calendar_year_2, format)  ->  calendar_year;

% Calendar month
get_date(calendar_month_1, input) ->  "0000-01";
get_date(calendar_month_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_date(calendar_month_1, parser)  ->  {date, {calendar_month, [{year,0},{month,1}]}};
get_date(calendar_month_1, value) ->  {0,1,1};
get_date(calendar_month_1, format) ->  calendar_month;
% Calendar month extended
get_date(calendar_month_2, input) ->  "2013-07";
get_date(calendar_month_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}];
get_date(calendar_month_2, parser)  ->  {date, {calendar_month, [{year,2013},{month,7}]}};
get_date(calendar_month_2, value) ->  {2013,7,1};
get_date(calendar_month_2, format) ->  calendar_month;

% Calendar
get_date(calendar_1, input) ->  "00000101";
get_date(calendar_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {decimal,"1",1}];
get_date(calendar_1, parser)  ->  {date, {calendar, [{year,0},{month,1},{monthday,1}]}};
get_date(calendar_1, value) ->  {0,1,1};
get_date(calendar_1, format) ->  calendar;
get_date(calendar_2, input) ->  "20130721";
get_date(calendar_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"7",7}, {decimal,"2",2}, {decimal,"1",1}];
get_date(calendar_2, parser)  ->  {date, {calendar, [{year,2013}, {month,7}, {monthday,21}]}};
get_date(calendar_2, value) ->  {2013,7,21};
get_date(calendar_2, format) ->  calendar;

% Calendar extended
get_date(calendar_extend_1, input)  ->  "0000-01-01";
get_date(calendar_extend_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_date(calendar_extend_1, parser) ->  {date, {calendar_extended, [{year,0},{month,1},{monthday,1}]}};
get_date(calendar_extend_1, value)  ->  {0,1,1};
get_date(calendar_extend_1, format)  ->  calendar_extended;
get_date(calendar_extend_2, input)  ->  "2013-07-21";
get_date(calendar_extend_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"1",1}];
get_date(calendar_extend_2, parser) ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,21}]}};
get_date(calendar_extend_2, value)  ->  {2013,7,21};
get_date(calendar_extend_2, format)  ->  calendar_extended;

% Ordinal
get_date(ordinal_1, input)  ->  "0000001";
get_date(ordinal_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_date(ordinal_1, parser) ->  {date, {ordinal,[{year,0},{yearday,1}]}};
get_date(ordinal_1, value)  ->  {0,1,1};
get_date(ordinal_1, format)  ->  ordinal;
get_date(ordinal_2, input)  ->  "2013202";
get_date(ordinal_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_date(ordinal_2, parser) ->  {date, {ordinal, [{year,2013}, {yearday,202}]}};
get_date(ordinal_2, value)  ->  {2013,7,21};
get_date(ordinal_2, format)  ->  ordinal;

% Ordinal extended
get_date(ordinal_extended_1, input) ->  "0000-001";
get_date(ordinal_extended_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_date(ordinal_extended_1, parser)  ->  {date, {ordinal_extended, [{year,0},{yearday,1}]}};
get_date(ordinal_extended_1, value) ->  {0,1,1};
get_date(ordinal_extended_1, format) ->  ordinal_extended;
get_date(ordinal_extended_2, input) ->  "2013-202";
get_date(ordinal_extended_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_date(ordinal_extended_2, parser)  ->  {date, {ordinal_extended, [{year,2013}, {yearday,202}]}};
get_date(ordinal_extended_2, value) ->  {2013,7,21};
get_date(ordinal_extended_2, format) ->  ordinal_extended;

% Week
get_date(week, input) ->  "2012W52";
get_date(week, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_date(week, parser)  ->  {date, {week, [{year,2012}, {weeknumber,52}]}};
get_date(week, value) ->  {2012,12,24};
get_date(week, format) -> week ;

% Week extended
get_date(week_extended, input)  ->  "2013-W29";
get_date(week_extended, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
get_date(week_extended, parser) ->  {date, {week_extended, [{year,2013}, {weeknumber,29}]}};
get_date(week_extended, value)  ->  {2013,7,15};
get_date(week_extended, format)  ->  week_extended;

% Weekday
get_date(weekday, input)  ->  "2013W297";
get_date(weekday, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {decimal,"7",7}];
get_date(weekday, parser) ->  {date, {weekday, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
get_date(weekday, value)  ->  {2013,7,21};
get_date(weekday, format)  ->  weekday;
% Weekday extended
get_date(weekday_extended, input) ->  "2009-W53-1";
get_date(weekday_extended, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}];
get_date(weekday_extended, parser)  ->  {date, {weekday_extended, [{year,2009}, {weeknumber,53}, {weekday,1}]}};
get_date(weekday_extended, value) ->  {2009,12,28};
get_date(weekday_extended, format) ->  weekday_extended;




% Others
%old get_date(16, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(16, parser)  ->  {date, {ordinal, [{year,2013},{yearday,1}]}};
%old get_date(16, value) ->  {2013,1,1};
%old get_date(17, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(17, parser)  ->  {date, {ordinal_extended, [{year,2013},{yearday,1}]}};
%old get_date(17, value) ->  {2013,1,1};
%old get_date(19, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"7",7}];
%old get_date(19, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
%old get_date(19, value) ->  {2013,7,21};
%old get_date(20, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {decimal,"1",1}];
%old get_date(20, parser)  ->  {date, {weekday, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
%old get_date(20, value) ->  {2013,1,7};
%old get_date(21, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_date(21, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
%old get_date(21, value) ->  {2013,1,7};
%old get_date(22, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
%old get_date(22, parser)  ->  {date, {week, [{year,2013}, {weeknumber,29}]}};
%old get_date(22, value) ->  {2013,7,15};
%old get_date(24, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
%old get_date(24, parser)  ->  {date, {week, [{year,2013}, {weeknumber,2}]}};
%old get_date(24, value) ->  {2013,1,7};
%old get_date(25, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
%old get_date(25, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,2}]}};
%old get_date(25, value) ->  {2013,1,7};
%old get_date(52, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(52, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,1}]}};
%old get_date(52, value) ->  {2013,1,1};
%old get_date(53, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(53, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,1}]}};
%old get_date(53, value) ->  {2013,2,1};
%old get_date(54, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(54, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,1}]}};
%old get_date(54, value) ->  {2013,3,1};
%old get_date(55, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(55, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,1}]}};
%old get_date(55, value) ->  {2013,4,1};
%old get_date(56, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(56, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,1}]}};
%old get_date(56, value) ->  {2013,5,1};
%old get_date(57, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"6",6}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(57, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,6}, {monthday,1}]}};
%old get_date(57, value) ->  {2013,6,1};
%old get_date(58, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(58, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,1}]}};
%old get_date(58, value) ->  {2013,7,1};
%old get_date(59, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(59, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,1}]}};
%old get_date(59, value) ->  {2013,8,1};
%old get_date(60, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(60, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,1}]}};
%old get_date(60, value) ->  {2013,9,1};
%old get_date(61, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(61, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,1}]}};
%old get_date(61, value) ->  {2013,10,1};
%old get_date(62, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(62, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,1}]}};
%old get_date(62, value) ->  {2013,11,1};
%old get_date(63, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(63, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,1}]}};
%old get_date(63, value) ->  {2013,12,1};
%old get_date(64, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(64, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,31}]}};
%old get_date(64, value) ->  {2013,1,31};
%old get_date(65, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
%old get_date(65, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,28}]}};
%old get_date(65, value) ->  {2013,2,28};
%old get_date(66, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(66, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,31}]}};
%old get_date(66, value) ->  {2013,3,31};
%old get_date(67, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_date(67, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,30}]}};
%old get_date(67, value) ->  {2013,4,30};
%old get_date(68, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(68, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,31}]}};
%old get_date(68, value) ->  {2013,5,31};
%old get_date(70, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(70, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,31}]}};
%old get_date(70, value) ->  {2013,7,31};
%old get_date(71, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(71, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,31}]}};
%old get_date(71, value) ->  {2013,8,31};
%old get_date(72, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_date(72, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,30}]}};
%old get_date(72, value) ->  {2013,9,30};
%old get_date(73, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(73, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,31}]}};
%old get_date(73, value) ->  {2013,10,31};
%old get_date(74, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_date(74, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,30}]}};
%old get_date(74, value) ->  {2013,11,30};
%old get_date(75, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_date(75, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,31}]}};
%old get_date(75, value) ->  {2013,12,31};
%old get_date(77, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_date(77, parser)  ->  {date, {weekday_extended, [{year,2010}, {weeknumber,52}, {weekday,1}]}};
%old get_date(77, value) ->  {2010,12,27};
%old get_date(78, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_date(78, parser)  ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,1}]}}; get_date(79, parser) ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,7}]}};
%old get_date(78, value) ->  {2011,12,26};
%old get_date(79, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"7",7}];
%old get_date(79, value) ->  {2012,1,1};
%old get_date(80, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_date(80, parser)  ->  {date, {weekday_extended, [{year,2012}, {weeknumber,52}, {weekday,1}]}};
%old get_date(80, value) ->  {2012,12,24};
%old get_date(81, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_date(81, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,52}, {weekday,1}]}};
%old get_date(81, value) ->  {2013,12,23};
%old get_date(82, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_date(82, parser)  ->  {date, {week_extended, [{year,2012}, {weeknumber,52}]}};
%old get_date(82, value) ->  {2012,12,24};
%old get_date(83, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_date(83, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,52}]}};
%old get_date(83, value) ->  {2013,12,23};
%old get_date(85, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_date(85, parser)  ->  {date, {week, [{year,2013}, {weeknumber,52}]}};
%old get_date(85, value) ->  {2013,12,23};
%old get_date(86, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(86, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,1}]}};
%old get_date(86, value) ->  {2012,2,1};
%old get_date(87, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"9",9}];
%old get_date(87, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,29}]}};
%old get_date(87, value) ->  {2012,2,29};
%old get_date(88, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"6",6}];
%old get_date(88, parser)  ->  {date, {ordinal_extended, [{year,2012}, {yearday,366}]}};
%old get_date(88, value) ->  {2012,12,31};
%old get_date(89, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_date(89, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,1}]}};
%old get_date(89, value) ->  {2011,2,1};
%old get_date(90, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
%old get_date(90, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,28}]}};
%old get_date(90, value) ->  {2011,2,28};
%old get_date(91, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"5",5}];
%old get_date(91, parser)  ->  {date, {ordinal_extended, [{year,2011}, {yearday,365}]}};
%old get_date(91, value) ->  {2011,12,31};
get_date(Test, Stage) ->  throw({'Unknonwn test/stage',Test,Stage}).


-spec(get_time(Test::atom(), Stage::input|lexer|parser|value|format )  ->  any()).
get_time(general_1, input)       ->     "T230550";
get_time(general_1, format)      ->     general;
get_time(general_1, lexer)       ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"2",2},
                                          {decimal,"3",3},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"5",5},
                                          {decimal,"0",0}],
                                         1};
get_time(general_1, parser)      ->     {ok,
                                         {time,
                                          {general,
                                           [{hour,23},
                                            {minute,5},
                                            {second,50}]}}};
get_time(general_1, value)       ->     {{23,5,50},0};
get_time(general_2, input)       ->     "T110501";
get_time(general_2, format)      ->     general;
get_time(general_2, lexer)       ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {decimal,"1",1}],
                                         1};
get_time(general_2, parser)      ->     {ok,
                                         {time,
                                          {general,
                                           [{hour,11},
                                            {minute,5},
                                            {second,1}]}}};
get_time(general_2, value)       ->     {{11,5,1},0};
get_time(general_extended_1, input)      ->     "T23:05:50";
get_time(general_extended_1, format)     ->     general_extended;
get_time(general_extended_1, lexer)      ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {time_separator,":",":"},
                                                  {decimal,"0",0},
                                                  {decimal,"5",5},
                                                  {time_separator,":",":"},
                                                  {decimal,"5",5},
                                                  {decimal,"0",0}],
                                                 1};
get_time(general_extended_1, parser)     ->     {ok,
                                                 {time,
                                                  {general_extended,
                                                   [{hour,23},
                                                    {minute,5},
                                                    {second,50}]}}};
get_time(general_extended_1, value)      ->     {{23,5,50},0};
get_time(general_extended_2, input)      ->     "T11:05:50";
get_time(general_extended_2, format)     ->     general_extended;
get_time(general_extended_2, lexer)      ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"1",1},
                                                  {decimal,"1",1},
                                                  {time_separator,":",":"},
                                                  {decimal,"0",0},
                                                  {decimal,"5",5},
                                                  {time_separator,":",":"},
                                                  {decimal,"5",5},
                                                  {decimal,"0",0}],
                                                 1};
get_time(general_extended_2, parser)     ->     {ok,
                                                 {time,
                                                  {general_extended,
                                                   [{hour,11},
                                                    {minute,5},
                                                    {second,50}]}}};
get_time(general_extended_2, value)      ->     {{11,5,50},0};
get_time(general_extended_frac_1, input)         ->     "T23:05:50,1212";
get_time(general_extended_frac_1, format)        ->     general_extended_frac;
get_time(general_extended_frac_1, lexer)         ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"5",5},
                                                          {decimal,"0",0},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2}],
                                                         1};
get_time(general_extended_frac_1, parser)        ->     {ok,
                                                         {time,
                                                          {general_extended_frac,
                                                           [{hour,23},
                                                            {minute,5},
                                                            {second,50},
                                                            {frac,1,
                                                             "1212"}]}}};
get_time(general_extended_frac_1, value)         ->     {{23,5,50},121200};
get_time(general_extended_frac_2, input)         ->     "T12:05:50,1212";
get_time(general_extended_frac_2, format)        ->     general_extended_frac;
get_time(general_extended_frac_2, lexer)         ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"5",5},
                                                          {decimal,"0",0},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2}],
                                                         1};
get_time(general_extended_frac_2, parser)        ->     {ok,
                                                         {time,
                                                          {general_extended_frac,
                                                           [{hour,12},
                                                            {minute,5},
                                                            {second,50},
                                                            {frac,1,
                                                             "1212"}]}}};
get_time(general_extended_frac_2, value)         ->     {{12,5,50},121200};
get_time(general_frac_1, input)  ->     "T100301,12";
get_time(general_frac_1, format)         ->     general_frac;
get_time(general_frac_1, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"0",0},
                                          {decimal,"3",3},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {frac_separator,",",","},
                                          {decimal,"1",1},
                                          {decimal,"2",2}],
                                         1};
get_time(general_frac_1, parser)         ->     {ok,
                                                 {time,
                                                  {general_frac,
                                                   [{hour,10},
                                                    {minute,3},
                                                    {second,1},
                                                    {frac,1,"12"}]}}};
get_time(general_frac_1, value)  ->     {{10,3,1},120000};
get_time(general_frac_2, input)  ->     "T230550,12";
get_time(general_frac_2, format)         ->     general_frac;
get_time(general_frac_2, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"2",2},
                                          {decimal,"3",3},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {frac_separator,",",","},
                                          {decimal,"1",1},
                                          {decimal,"2",2}],
                                         1};
get_time(general_frac_2, parser)         ->     {ok,
                                                 {time,
                                                  {general_frac,
                                                   [{hour,23},
                                                    {minute,5},
                                                    {second,50},
                                                    {frac,1,"12"}]}}};
get_time(general_frac_2, value)  ->     {{23,5,50},120000};
get_time(general_hour_1, input)  ->     "T23";
get_time(general_hour_1, format)         ->     general_hour;
get_time(general_hour_1, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"2",2},
                                          {decimal,"3",3}],
                                         1};
get_time(general_hour_1, parser)         ->     {ok,
                                                 {time,
                                                  {general_hour,[{hour,23}]}}};
get_time(general_hour_1, value)  ->     {{23,0,0},0};
get_time(general_hour_2, input)  ->     "T11";
get_time(general_hour_2, format)         ->     general_hour;
get_time(general_hour_2, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1}],
                                         1};
get_time(general_hour_2, parser)         ->     {ok,
                                                 {time,
                                                  {general_hour,[{hour,11}]}}};
get_time(general_hour_2, value)  ->     {{11,0,0},0};
get_time(general_hour_frac_1, input)     ->     "T23,12";
get_time(general_hour_frac_1, format)    ->     general_hour_frac;
get_time(general_hour_frac_1, lexer)     ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {frac_separator,",",","},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2}],
                                                 1};
get_time(general_hour_frac_1, parser)    ->     {ok,
                                                 {time,
                                                  {general_hour_frac,
                                                   [{hour,23},
                                                    {frac,3600,"12"}]}}};
get_time(general_hour_frac_1, value)     ->     {{23,7,12},0};
get_time(general_hour_frac_2, input)     ->     "T10,12";
get_time(general_hour_frac_2, format)    ->     general_hour_frac;
get_time(general_hour_frac_2, lexer)     ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"1",1},
                                                  {decimal,"0",0},
                                                  {frac_separator,",",","},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2}],
                                                 1};
get_time(general_hour_frac_2, parser)    ->     {ok,
                                                 {time,
                                                  {general_hour_frac,
                                                   [{hour,10},
                                                    {frac,3600,"12"}]}}};
get_time(general_hour_frac_2, value)     ->     {{10,7,12},0};
get_time(general_minute_1, input)        ->     "T2305";
get_time(general_minute_1, format)       ->     general_minute;
get_time(general_minute_1, lexer)        ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {decimal,"0",0},
                                                  {decimal,"5",5}],
                                                 1};
get_time(general_minute_1, parser)       ->     {ok,
                                                 {time,
                                                  {general_minute,
                                                   [{hour,23},{minute,5}]}}};
get_time(general_minute_1, value)        ->     {{23,5,0},0};
get_time(general_minute_2, input)        ->     "T1112";
get_time(general_minute_2, format)       ->     general_minute;
get_time(general_minute_2, lexer)        ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"1",1},
                                                  {decimal,"1",1},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2}],
                                                 1};
get_time(general_minute_2, parser)       ->     {ok,
                                                 {time,
                                                  {general_minute,
                                                   [{hour,11},{minute,12}]}}};
get_time(general_minute_2, value)        ->     {{11,12,0},0};
get_time(general_minute_extended_1, input)       ->     "T23:05";
get_time(general_minute_extended_1, format)      ->     general_minute_extended;
get_time(general_minute_extended_1, lexer)       ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5}],
                                                         1};
get_time(general_minute_extended_1, parser)      ->     {ok,
                                                         {time,
                                                          {general_minute_extended,
                                                           [{hour,23},
                                                            {minute,5}]}}};
get_time(general_minute_extended_1, value)       ->     {{23,5,0},0};
get_time(general_minute_extended_2, input)       ->     "T10:05";
get_time(general_minute_extended_2, format)      ->     general_minute_extended;
get_time(general_minute_extended_2, lexer)       ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5}],
                                                         1};
get_time(general_minute_extended_2, parser)      ->     {ok,
                                                         {time,
                                                          {general_minute_extended,
                                                           [{hour,10},
                                                            {minute,5}]}}};
get_time(general_minute_extended_2, value)       ->     {{10,5,0},0};
get_time(general_minute_extended_frac_1, input)  ->     "T10:05,12";
get_time(general_minute_extended_frac_1, format)         ->     general_minute_extended_frac;
get_time(general_minute_extended_frac_1, lexer)  ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2}],
                                                         1};
get_time(general_minute_extended_frac_1, parser)         ->     {ok,
                                                                 {time,
                                                                  {general_minute_extended_frac,
                                                                   [{hour,10},
                                                                    {minute,5},
                                                                    {frac,60,
                                                                     "12"}]}}};
get_time(general_minute_extended_frac_1, value)  ->     {{10,5,7},200000};
get_time(general_minute_extended_frac_2, input)  ->     "T23:05,12";
get_time(general_minute_extended_frac_2, format)         ->     general_minute_extended_frac;
get_time(general_minute_extended_frac_2, lexer)  ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2}],
                                                         1};
get_time(general_minute_extended_frac_2, parser)         ->     {ok,
                                                                 {time,
                                                                  {general_minute_extended_frac,
                                                                   [{hour,23},
                                                                    {minute,5},
                                                                    {frac,60,
                                                                     "12"}]}}};
get_time(general_minute_extended_frac_2, value)  ->     {{23,5,7},200000};
get_time(general_minute_frac_1, input)   ->     "T1201,12";
get_time(general_minute_frac_1, format)  ->     general_minute_frac;
get_time(general_minute_frac_1, lexer)   ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2},
                                                  {decimal,"0",0},
                                                  {decimal,"1",1},
                                                  {frac_separator,",",","},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2}],
                                                 1};
get_time(general_minute_frac_1, parser)  ->     {ok,
                                                 {time,
                                                  {general_minute_frac,
                                                   [{hour,12},
                                                    {minute,1},
                                                    {frac,60,"12"}]}}};
get_time(general_minute_frac_1, value)   ->     {{12,1,7},200000};
get_time(general_minute_frac_2, input)   ->     "T2305,12";
get_time(general_minute_frac_2, format)  ->     general_minute_frac;
get_time(general_minute_frac_2, lexer)   ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {decimal,"0",0},
                                                  {decimal,"5",5},
                                                  {frac_separator,",",","},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2}],
                                                 1};
get_time(general_minute_frac_2, parser)  ->     {ok,
                                                 {time,
                                                  {general_minute_frac,
                                                   [{hour,23},
                                                    {minute,5},
                                                    {frac,60,"12"}]}}};
get_time(general_minute_frac_2, value)   ->     {{23,5,7},200000};


get_time(Test, Stage) ->  throw({'Unknonwn test/stage',Test,Stage}).


-spec(get_localtime(Test::atom(), Stage::input|lexer|parser|value|format )  ->  any()).
get_localtime(general_1, input)  ->     "T230550+0115";
get_localtime(general_1, format)         ->     general;
get_localtime(general_1, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"2",2},
                                          {decimal,"3",3},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {plus,"+","+"},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"5",5}],
                                         1};
get_localtime(general_1, parser)         ->     {ok,
                                                 {localtime,
                                                  {general,
                                                   [{hour,23},
                                                    {minute,5},
                                                    {second,50}]},
                                                  {minute,add,
                                                   [{hour,1},{minute,15}]}}};
get_localtime(general_1, value)         ->     {{23,5,50},0,(1*60+15)*60};
get_localtime(general_2, input)  ->     "T110501-1001";
get_localtime(general_2, format)         ->     general;
get_localtime(general_2, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {minus,"-","-"},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"0",0},
                                          {decimal,"1",1}],
                                         1};
get_localtime(general_2, parser)         ->     {ok,
                                                 {localtime,
                                                  {general,
                                                   [{hour,11},
                                                    {minute,5},
                                                    {second,1}]},
                                                  {minute,sub,
                                                   [{hour,10},{minute,1}]}}};
get_localtime(general_3, input)  ->     "T110501Z";
get_localtime(general_3, format)         ->     general;
get_localtime(general_3, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {timezone_utc,"Z","Z"}],
                                         1};
get_localtime(general_3, parser)         ->     {ok,
                                                 {localtime,
                                                  {general,
                                                   [{hour,11},
                                                    {minute,5},
                                                    {second,1}]},
                                                  {utc,add,[]}}};
get_localtime(general_4, input)  ->     "T110501-10";
get_localtime(general_4, format)         ->     general;
get_localtime(general_4, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {minus,"-","-"},
                                          {decimal,"1",1},
                                          {decimal,"0",0}],
                                         1};
get_localtime(general_4, parser)         ->     {ok,
                                                 {localtime,
                                                  {general,
                                                   [{hour,11},
                                                    {minute,5},
                                                    {second,1}]},
                                                  {hour,sub,[{hour,10}]}}};
get_localtime(general_5, input)  ->     "T110501+05";
get_localtime(general_5, format)         ->     general;
get_localtime(general_5, lexer)  ->     {ok,
                                         [{time_designator,"T","T"},
                                          {decimal,"1",1},
                                          {decimal,"1",1},
                                          {decimal,"0",0},
                                          {decimal,"5",5},
                                          {decimal,"0",0},
                                          {decimal,"1",1},
                                          {plus,"+","+"},
                                          {decimal,"0",0},
                                          {decimal,"5",5}],
                                         1};
get_localtime(general_5, parser)         ->     {ok,
                                                 {localtime,
                                                  {general,
                                                   [{hour,11},
                                                    {minute,5},
                                                    {second,1}]},
                                                  {hour,add,[{hour,5}]}}};
get_localtime(general_extended_1, input)         ->     "T23:05:50+10:00";
get_localtime(general_extended_1, format)        ->     general_extended;
get_localtime(general_extended_1, lexer)         ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"5",5},
                                                          {decimal,"0",0},
                                                          {plus,"+","+"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0}],
                                                         1};
get_localtime(general_extended_1, parser)        ->     {ok,
                                                         {localtime,
                                                          {general_extended,
                                                           [{hour,23},
                                                            {minute,5},
                                                            {second,50}]},
                                                          {minute_extended,add,
                                                           [{hour,10},
                                                            {minute,0}]}}};
get_localtime(general_extended_frac_1, input)    ->     "T23:05:50,1212+10:00";
get_localtime(general_extended_frac_1, format)   ->     general_extended_frac;
get_localtime(general_extended_frac_1, lexer)    ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"5",5},
                                                          {decimal,"0",0},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {plus,"+","+"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0}],
                                                         1};
get_localtime(general_extended_frac_1, parser)   ->     {ok,
                                                         {localtime,
                                                          {general_extended_frac,
                                                           [{hour,23},
                                                            {minute,5},
                                                            {second,50},
                                                            {frac,1,"1212"}]},
                                                          {minute_extended,add,
                                                           [{hour,10},
                                                            {minute,0}]}}};
get_localtime(general_frac_1, input)     ->     "T100301,12+1000";
get_localtime(general_frac_1, format)    ->     general_frac;
get_localtime(general_frac_1, lexer)     ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"1",1},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0},
                                                  {decimal,"3",3},
                                                  {decimal,"0",0},
                                                  {decimal,"1",1},
                                                  {frac_separator,",",","},
                                                  {decimal,"1",1},
                                                  {decimal,"2",2},
                                                  {plus,"+","+"},
                                                  {decimal,"1",1},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0}],
                                                 1};
get_localtime(general_frac_1, parser)    ->     {ok,
                                                 {localtime,
                                                  {general_frac,
                                                   [{hour,10},
                                                    {minute,3},
                                                    {second,1},
                                                    {frac,1,"12"}]},
                                                  {minute,add,
                                                   [{hour,10},{minute,0}]}}};
get_localtime(general_hour_1, input)     ->     "T23+1000";
get_localtime(general_hour_1, format)    ->     general_hour;
get_localtime(general_hour_1, lexer)     ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {plus,"+","+"},
                                                  {decimal,"1",1},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0}],
                                                 1};
get_localtime(general_hour_1, parser)    ->     {ok,
                                                 {localtime,
                                                  {general_hour,[{hour,23}]},
                                                  {minute,add,
                                                   [{hour,10},{minute,0}]}}};
get_localtime(general_hour_frac_1, input)        ->     "T23,12+1000";
get_localtime(general_hour_frac_1, format)       ->     general_hour_frac;
get_localtime(general_hour_frac_1, lexer)        ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {plus,"+","+"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0}],
                                                         1};
get_localtime(general_hour_frac_1, parser)       ->     {ok,
                                                         {localtime,
                                                          {general_hour_frac,
                                                           [{hour,23},
                                                            {frac,3600,"12"}]},
                                                          {minute,add,
                                                           [{hour,10},
                                                            {minute,0}]}}};
get_localtime(general_minute_1, input)   ->     "T2305+1000";
get_localtime(general_minute_1, format)  ->     general_minute;
get_localtime(general_minute_1, lexer)   ->     {ok,
                                                 [{time_designator,"T","T"},
                                                  {decimal,"2",2},
                                                  {decimal,"3",3},
                                                  {decimal,"0",0},
                                                  {decimal,"5",5},
                                                  {plus,"+","+"},
                                                  {decimal,"1",1},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0},
                                                  {decimal,"0",0}],
                                                 1};
get_localtime(general_minute_1, parser)  ->     {ok,
                                                 {localtime,
                                                  {general_minute,
                                                   [{hour,23},{minute,5}]},
                                                  {minute,add,
                                                   [{hour,10},{minute,0}]}}};
get_localtime(general_minute_extended_1, input)  ->     "T23:05+10:00";
get_localtime(general_minute_extended_1, format)         ->     general_minute_extended;
get_localtime(general_minute_extended_1, lexer)  ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"2",2},
                                                          {decimal,"3",3},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"5",5},
                                                          {plus,"+","+"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {time_separator,
                                                           ":",":"},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0}],
                                                         1};
get_localtime(general_minute_extended_1, parser)         ->     {ok,
                                                                 {localtime,
                                                                  {general_minute_extended,
                                                                   [{hour,23},
                                                                    {minute,
                                                                     5}]},
                                                                  {minute_extended,
                                                                   add,
                                                                   [{hour,10},
                                                                    {minute,
                                                                     0}]}}};
get_localtime(general_minute_extended_frac_1, input)     ->     "T10:05,12+10:00";
get_localtime(general_minute_extended_frac_1, format)    ->     general_minute_extended_frac;
get_localtime(general_minute_extended_frac_1, lexer)     ->     {ok,
                                                                 [{time_designator,
                                                                   "T","T"},
                                                                  {decimal,
                                                                   "1",1},
                                                                  {decimal,
                                                                   "0",0},
                                                                  {time_separator,
                                                                   ":",":"},
                                                                  {decimal,
                                                                   "0",0},
                                                                  {decimal,
                                                                   "5",5},
                                                                  {frac_separator,
                                                                   ",",","},
                                                                  {decimal,
                                                                   "1",1},
                                                                  {decimal,
                                                                   "2",2},
                                                                  {plus,"+",
                                                                   "+"},
                                                                  {decimal,
                                                                   "1",1},
                                                                  {decimal,
                                                                   "0",0},
                                                                  {time_separator,
                                                                   ":",":"},
                                                                  {decimal,
                                                                   "0",0},
                                                                  {decimal,
                                                                   "0",0}],
                                                                 1};
get_localtime(general_minute_extended_frac_1, parser)    ->     {ok,
                                                                 {localtime,
                                                                  {general_minute_extended_frac,
                                                                   [{hour,10},
                                                                    {minute,5},
                                                                    {frac,60,
                                                                     "12"}]},
                                                                  {minute_extended,
                                                                   add,
                                                                   [{hour,10},
                                                                    {minute,
                                                                     0}]}}};
get_localtime(general_minute_frac_1, input)      ->     "T1201,12+1000";
get_localtime(general_minute_frac_1, format)     ->     general_minute_frac;
get_localtime(general_minute_frac_1, lexer)      ->     {ok,
                                                         [{time_designator,
                                                           "T","T"},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {decimal,"0",0},
                                                          {decimal,"1",1},
                                                          {frac_separator,
                                                           ",",","},
                                                          {decimal,"1",1},
                                                          {decimal,"2",2},
                                                          {plus,"+","+"},
                                                          {decimal,"1",1},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0},
                                                          {decimal,"0",0}],
                                                         1};
get_localtime(general_minute_frac_1, parser)     ->     {ok,
                                                         {localtime,
                                                          {general_minute_frac,
                                                           [{hour,12},
                                                            {minute,1},
                                                            {frac,60,"12"}]},
                                                          {minute,add,
                                                           [{hour,10},
                                                            {minute,0}]}}};


get_localtime(Test, Stage) ->  throw({'Unknonwn test/stage',Test,Stage}).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%% Internal functions



%% vim: set ts=4 sw=4 ai invlist si cul nu:
