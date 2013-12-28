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
-export([get_parse_date_stage/2, run_stages/0]).

%%% API
run_stages()  ->
    lists:foreach(
  fun(X)  ->
    try
  {ok, Result} = iso8601_parser:parse(get_parse_date_stage(X,lexer)),
  io:format("get_parse_date_stage(~p, parser)\t ->  \t~p;\n",[X,Result])
    catch
  error:_ ->  ok;
  throw:_ ->  ok;
  exit:_  ->  ok
    end
  end,
  lists:seq(1,91)
     ).

-spec(get_parse_date_stage(Test::atom(), Stage::input|lexer|parser|value|format )  ->  any()).

% Calendar century
get_parse_date_stage(calendar_century_1, input) ->  "00";
get_parse_date_stage(calendar_century_1, lexer) ->  [{decimal,"0",0},{decimal,"0",0}];
get_parse_date_stage(calendar_century_1, parser)  ->  {date, {calendar_century,[{century,0}]}};
get_parse_date_stage(calendar_century_1, value) ->  {1,1,1};
get_parse_date_stage(calendar_century_1, format) ->  calendar_century;
get_parse_date_stage(calendar_century_2, input) ->  "20";
get_parse_date_stage(calendar_century_2, lexer) ->  [{decimal,"2",2},{decimal,"0",0}];
get_parse_date_stage(calendar_century_2, parser)  ->  {date, {calendar_century,[{century,20}]}};
get_parse_date_stage(calendar_century_2, value) ->  {2001,1,1};
get_parse_date_stage(calendar_century_2, format) ->  calendar_century;

% Calendar year
get_parse_date_stage(calendar_year_1, input)  ->  "0000";
get_parse_date_stage(calendar_year_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}];
get_parse_date_stage(calendar_year_1, parser) ->  {date,{calendar_year,[{year,0}]}};
get_parse_date_stage(calendar_year_1, value)  ->  {0,1,1};
get_parse_date_stage(calendar_year_1, format)  ->  calendar_year;
get_parse_date_stage(calendar_year_2, input)  ->  "2013";
get_parse_date_stage(calendar_year_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}];
get_parse_date_stage(calendar_year_2, parser) ->  {date, {calendar_year, [{year,2013}]}};
get_parse_date_stage(calendar_year_2, value)  ->  {2013,1,1};
get_parse_date_stage(calendar_year_2, format)  ->  calendar_year;

% Calendar month
get_parse_date_stage(calendar_month_1, input) ->  "0000-01";
get_parse_date_stage(calendar_month_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_month_1, parser)  ->  {date, {calendar_month, [{year,0},{month,1}]}};
get_parse_date_stage(calendar_month_1, value) ->  {0,1,1};
get_parse_date_stage(calendar_month_1, format) ->  calendar_month;
% Calendar month extended
get_parse_date_stage(calendar_month_2, input) ->  "2013-07";
get_parse_date_stage(calendar_month_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}];
get_parse_date_stage(calendar_month_2, parser)  ->  {date, {calendar_month, [{year,2013},{month,7}]}};
get_parse_date_stage(calendar_month_2, value) ->  {2013,7,1};
get_parse_date_stage(calendar_month_2, format) ->  calendar_month;

% Calendar
get_parse_date_stage(calendar_1, input) ->  "00000101";
get_parse_date_stage(calendar_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_1, parser)  ->  {date, {calendar, [{year,0},{month,1},{monthday,1}]}};
get_parse_date_stage(calendar_1, value) ->  {0,1,1};
get_parse_date_stage(calendar_1, format) ->  calendar;
get_parse_date_stage(calendar_2, input) ->  "20130721";
get_parse_date_stage(calendar_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"7",7}, {decimal,"2",2}, {decimal,"1",1}];
get_parse_date_stage(calendar_2, parser)  ->  {date, {calendar, [{year,2013}, {month,7}, {monthday,21}]}};
get_parse_date_stage(calendar_2, value) ->  {2013,7,21};
get_parse_date_stage(calendar_2, format) ->  calendar;

% Calendar extended
get_parse_date_stage(calendar_extend_1, input)  ->  "0000-01-01";
get_parse_date_stage(calendar_extend_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_extend_1, parser) ->  {date, {calendar_extended, [{year,0},{month,1},{monthday,1}]}};
get_parse_date_stage(calendar_extend_1, value)  ->  {0,1,1};
get_parse_date_stage(calendar_extend_1, format)  ->  calendar_extended;
get_parse_date_stage(calendar_extend_2, input)  ->  "2013-07-21";
get_parse_date_stage(calendar_extend_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"1",1}];
get_parse_date_stage(calendar_extend_2, parser) ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,21}]}};
get_parse_date_stage(calendar_extend_2, value)  ->  {2013,7,21};
get_parse_date_stage(calendar_extend_2, format)  ->  calendar_extended;

% Ordinal
get_parse_date_stage(ordinal_1, input)  ->  "0000001";
get_parse_date_stage(ordinal_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(ordinal_1, parser) ->  {date, {ordinal,[{year,0},{yearday,1}]}};
get_parse_date_stage(ordinal_1, value)  ->  {0,1,1};
get_parse_date_stage(ordinal_1, format)  ->  ordinal;
get_parse_date_stage(ordinal_2, input)  ->  "2013202";
get_parse_date_stage(ordinal_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(ordinal_2, parser) ->  {date, {ordinal, [{year,2013}, {yearday,202}]}};
get_parse_date_stage(ordinal_2, value)  ->  {2013,7,21};
get_parse_date_stage(ordinal_2, format)  ->  ordinal;

% Ordinal extended
get_parse_date_stage(ordinal_extended_1, input) ->  "0000-001";
get_parse_date_stage(ordinal_extended_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(ordinal_extended_1, parser)  ->  {date, {ordinal_extended, [{year,0},{yearday,1}]}};
get_parse_date_stage(ordinal_extended_1, value) ->  {0,1,1};
get_parse_date_stage(ordinal_extended_1, format) ->  ordinal_extended;
get_parse_date_stage(ordinal_extended_2, input) ->  "2013-202";
get_parse_date_stage(ordinal_extended_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(ordinal_extended_2, parser)  ->  {date, {ordinal_extended, [{year,2013}, {yearday,202}]}};
get_parse_date_stage(ordinal_extended_2, value) ->  {2013,7,21};
get_parse_date_stage(ordinal_extended_2, format) ->  ordinal_extended;

% Week
get_parse_date_stage(week, input) ->  "2012W52";
get_parse_date_stage(week, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_parse_date_stage(week, parser)  ->  {date, {week, [{year,2012}, {weeknumber,52}]}};
get_parse_date_stage(week, value) ->  {2012,12,24};
get_parse_date_stage(week, format) -> week ;

% Week extended
get_parse_date_stage(week_extended, input)  ->  "2013-W29";
get_parse_date_stage(week_extended, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
get_parse_date_stage(week_extended, parser) ->  {date, {week_extended, [{year,2013}, {weeknumber,29}]}};
get_parse_date_stage(week_extended, value)  ->  {2013,7,15};
get_parse_date_stage(week_extended, format)  ->  week_extended;

% Weekday
get_parse_date_stage(weekday, input)  ->  "2013W297";
get_parse_date_stage(weekday, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {decimal,"7",7}];
get_parse_date_stage(weekday, parser) ->  {date, {weekday, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
get_parse_date_stage(weekday, value)  ->  {2013,7,21};
get_parse_date_stage(weekday, format)  ->  weekday;
% Weekday extended
get_parse_date_stage(weekday_extended, input) ->  "2009-W53-1";
get_parse_date_stage(weekday_extended, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(weekday_extended, parser)  ->  {date, {weekday_extended, [{year,2009}, {weeknumber,53}, {weekday,1}]}};
get_parse_date_stage(weekday_extended, value) ->  {2009,12,28};
get_parse_date_stage(weekday_extended, format) ->  weekday_extended;




% Others
%old get_parse_date_stage(16, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(16, parser)  ->  {date, {ordinal, [{year,2013},{yearday,1}]}};
%old get_parse_date_stage(16, value) ->  {2013,1,1};
%old get_parse_date_stage(17, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(17, parser)  ->  {date, {ordinal_extended, [{year,2013},{yearday,1}]}};
%old get_parse_date_stage(17, value) ->  {2013,1,1};
%old get_parse_date_stage(19, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"7",7}];
%old get_parse_date_stage(19, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
%old get_parse_date_stage(19, value) ->  {2013,7,21};
%old get_parse_date_stage(20, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {decimal,"1",1}];
%old get_parse_date_stage(20, parser)  ->  {date, {weekday, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
%old get_parse_date_stage(20, value) ->  {2013,1,7};
%old get_parse_date_stage(21, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_parse_date_stage(21, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
%old get_parse_date_stage(21, value) ->  {2013,1,7};
%old get_parse_date_stage(22, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
%old get_parse_date_stage(22, parser)  ->  {date, {week, [{year,2013}, {weeknumber,29}]}};
%old get_parse_date_stage(22, value) ->  {2013,7,15};
%old get_parse_date_stage(24, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
%old get_parse_date_stage(24, parser)  ->  {date, {week, [{year,2013}, {weeknumber,2}]}};
%old get_parse_date_stage(24, value) ->  {2013,1,7};
%old get_parse_date_stage(25, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
%old get_parse_date_stage(25, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,2}]}};
%old get_parse_date_stage(25, value) ->  {2013,1,7};
%old get_parse_date_stage(52, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(52, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,1}]}};
%old get_parse_date_stage(52, value) ->  {2013,1,1};
%old get_parse_date_stage(53, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(53, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,1}]}};
%old get_parse_date_stage(53, value) ->  {2013,2,1};
%old get_parse_date_stage(54, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(54, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,1}]}};
%old get_parse_date_stage(54, value) ->  {2013,3,1};
%old get_parse_date_stage(55, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(55, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,1}]}};
%old get_parse_date_stage(55, value) ->  {2013,4,1};
%old get_parse_date_stage(56, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(56, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,1}]}};
%old get_parse_date_stage(56, value) ->  {2013,5,1};
%old get_parse_date_stage(57, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"6",6}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(57, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,6}, {monthday,1}]}};
%old get_parse_date_stage(57, value) ->  {2013,6,1};
%old get_parse_date_stage(58, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(58, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,1}]}};
%old get_parse_date_stage(58, value) ->  {2013,7,1};
%old get_parse_date_stage(59, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(59, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,1}]}};
%old get_parse_date_stage(59, value) ->  {2013,8,1};
%old get_parse_date_stage(60, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(60, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,1}]}};
%old get_parse_date_stage(60, value) ->  {2013,9,1};
%old get_parse_date_stage(61, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(61, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,1}]}};
%old get_parse_date_stage(61, value) ->  {2013,10,1};
%old get_parse_date_stage(62, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(62, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,1}]}};
%old get_parse_date_stage(62, value) ->  {2013,11,1};
%old get_parse_date_stage(63, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(63, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,1}]}};
%old get_parse_date_stage(63, value) ->  {2013,12,1};
%old get_parse_date_stage(64, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(64, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,31}]}};
%old get_parse_date_stage(64, value) ->  {2013,1,31};
%old get_parse_date_stage(65, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
%old get_parse_date_stage(65, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,28}]}};
%old get_parse_date_stage(65, value) ->  {2013,2,28};
%old get_parse_date_stage(66, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(66, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,31}]}};
%old get_parse_date_stage(66, value) ->  {2013,3,31};
%old get_parse_date_stage(67, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_parse_date_stage(67, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,30}]}};
%old get_parse_date_stage(67, value) ->  {2013,4,30};
%old get_parse_date_stage(68, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(68, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,31}]}};
%old get_parse_date_stage(68, value) ->  {2013,5,31};
%old get_parse_date_stage(70, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(70, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,31}]}};
%old get_parse_date_stage(70, value) ->  {2013,7,31};
%old get_parse_date_stage(71, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(71, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,31}]}};
%old get_parse_date_stage(71, value) ->  {2013,8,31};
%old get_parse_date_stage(72, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_parse_date_stage(72, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,30}]}};
%old get_parse_date_stage(72, value) ->  {2013,9,30};
%old get_parse_date_stage(73, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(73, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,31}]}};
%old get_parse_date_stage(73, value) ->  {2013,10,31};
%old get_parse_date_stage(74, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
%old get_parse_date_stage(74, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,30}]}};
%old get_parse_date_stage(74, value) ->  {2013,11,30};
%old get_parse_date_stage(75, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
%old get_parse_date_stage(75, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,31}]}};
%old get_parse_date_stage(75, value) ->  {2013,12,31};
%old get_parse_date_stage(77, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_parse_date_stage(77, parser)  ->  {date, {weekday_extended, [{year,2010}, {weeknumber,52}, {weekday,1}]}};
%old get_parse_date_stage(77, value) ->  {2010,12,27};
%old get_parse_date_stage(78, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_parse_date_stage(78, parser)  ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,1}]}}; get_parse_date_stage(79, parser) ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,7}]}};
%old get_parse_date_stage(78, value) ->  {2011,12,26};
%old get_parse_date_stage(79, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"7",7}];
%old get_parse_date_stage(79, value) ->  {2012,1,1};
%old get_parse_date_stage(80, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_parse_date_stage(80, parser)  ->  {date, {weekday_extended, [{year,2012}, {weeknumber,52}, {weekday,1}]}};
%old get_parse_date_stage(80, value) ->  {2012,12,24};
%old get_parse_date_stage(81, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
%old get_parse_date_stage(81, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,52}, {weekday,1}]}};
%old get_parse_date_stage(81, value) ->  {2013,12,23};
%old get_parse_date_stage(82, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_parse_date_stage(82, parser)  ->  {date, {week_extended, [{year,2012}, {weeknumber,52}]}};
%old get_parse_date_stage(82, value) ->  {2012,12,24};
%old get_parse_date_stage(83, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_parse_date_stage(83, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,52}]}};
%old get_parse_date_stage(83, value) ->  {2013,12,23};
%old get_parse_date_stage(85, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
%old get_parse_date_stage(85, parser)  ->  {date, {week, [{year,2013}, {weeknumber,52}]}};
%old get_parse_date_stage(85, value) ->  {2013,12,23};
%old get_parse_date_stage(86, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(86, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,1}]}};
%old get_parse_date_stage(86, value) ->  {2012,2,1};
%old get_parse_date_stage(87, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"9",9}];
%old get_parse_date_stage(87, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,29}]}};
%old get_parse_date_stage(87, value) ->  {2012,2,29};
%old get_parse_date_stage(88, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"6",6}];
%old get_parse_date_stage(88, parser)  ->  {date, {ordinal_extended, [{year,2012}, {yearday,366}]}};
%old get_parse_date_stage(88, value) ->  {2012,12,31};
%old get_parse_date_stage(89, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
%old get_parse_date_stage(89, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,1}]}};
%old get_parse_date_stage(89, value) ->  {2011,2,1};
%old get_parse_date_stage(90, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
%old get_parse_date_stage(90, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,28}]}};
%old get_parse_date_stage(90, value) ->  {2011,2,28};
%old get_parse_date_stage(91, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"5",5}];
%old get_parse_date_stage(91, parser)  ->  {date, {ordinal_extended, [{year,2011}, {yearday,365}]}};
%old get_parse_date_stage(91, value) ->  {2011,12,31};
get_parse_date_stage(Test, Stage) ->  throw({'Unknonwn test/stage',Test,Stage}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%% Internal functions



%% vim: set ts=4 sw=4 ai invlist si cul nu:
