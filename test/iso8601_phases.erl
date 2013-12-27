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

-spec(get_parse_date_stage(Test::atom(), Stage::input|lexer|parser|value )  ->  any()).

% Calendar century
get_parse_date_stage(calendar_century_1, lexer) ->  [{decimal,"0",0},{decimal,"0",0}];
get_parse_date_stage(calendar_century_1, parser)  ->  {date, {calendar_century,[{century,0}]}};
get_parse_date_stage(calendar_century_1, value) ->  {1,1,1};
get_parse_date_stage(calendar_century_2, lexer) ->  [{decimal,"2",2},{decimal,"0",0}];
get_parse_date_stage(calendar_century_2, parser)  ->  {date, {calendar_century,[{century,20}]}};
get_parse_date_stage(calendar_century_2, value) ->  {2001,1,1};

% Calendar year
get_parse_date_stage(calendar_year_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}];
get_parse_date_stage(calendar_year_1, parser) ->  {date,{calendar_year,[{year,0}]}};
get_parse_date_stage(calendar_year_1, value)  ->  {0,1,1};
get_parse_date_stage(calendar_year_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}];
get_parse_date_stage(calendar_year_2, parser) ->  {date, {calendar_year, [{year,2013}]}};
get_parse_date_stage(calendar_year_2, value)  ->  {2013,1,1};

% Calendar month
get_parse_date_stage(calendar_month_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_month_1, parser)  ->  {date, {calendar_month, [{year,0},{month,1}]}};
get_parse_date_stage(calendar_month_1, value) ->  {0,1,1};
% Calendar month extended
get_parse_date_stage(calendar_month_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}];
get_parse_date_stage(calendar_month_2, parser)  ->  {date, {calendar_month, [{year,2013},{month,7}]}};
get_parse_date_stage(calendar_month_2, value) ->  {2013,7,1};

% Calendar
get_parse_date_stage(calendar_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_1, parser)  ->  {date, {calendar, [{year,0},{month,1},{monthday,1}]}};
get_parse_date_stage(calendar_1, value) ->  {0,1,1};
get_parse_date_stage(calendar_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"7",7}, {decimal,"2",2}, {decimal,"1",1}];
get_parse_date_stage(calendar_2, parser)  ->  {date, {calendar, [{year,2013}, {month,7}, {monthday,21}]}};
get_parse_date_stage(calendar_2, value) ->  {2013,7,21};

% Calendar extended
get_parse_date_stage(calendar_extend_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(calendar_extend_1, parser) ->  {date, {calendar_extended, [{year,0},{month,1},{monthday,1}]}};
get_parse_date_stage(calendar_extend_1, value)  ->  {0,1,1};
get_parse_date_stage(calendar_extend_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"1",1}];
get_parse_date_stage(calendar_extend_2, parser) ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,21}]}};
get_parse_date_stage(calendar_extend_2, value)  ->  {2013,7,21};

% Ordinal
get_parse_date_stage(ordinal_1, lexer)  ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(ordinal_1, parser) ->  {date, {ordinal,[{year,0},{yearday,1}]}};
get_parse_date_stage(ordinal_1, value)  ->  {0,1,1};
get_parse_date_stage(ordinal_2, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(ordinal_2, parser) ->  {date, {ordinal, [{year,2013}, {yearday,202}]}};
get_parse_date_stage(ordinal_2, value)  ->  {2013,7,21};

% Ordinal extended
get_parse_date_stage(ordinal_extended_1, lexer) ->  [{decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(ordinal_extended_1, parser)  ->  {date, {ordinal_extended, [{year,0},{yearday,1}]}};
get_parse_date_stage(ordinal_extended_1, value) ->  {0,1,1};
get_parse_date_stage(ordinal_extended_2, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(ordinal_extended_2, parser)  ->  {date, {ordinal_extended, [{year,2013}, {yearday,202}]}};
get_parse_date_stage(ordinal_extended_2, value) ->  {2013,7,21};

% Week
get_parse_date_stage(ordinal_week, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_parse_date_stage(ordinal_week, parser)  ->  {date, {week, [{year,2012}, {weeknumber,52}]}};
get_parse_date_stage(ordinal_week, value) ->  {2012,12,24};

% Week extended
get_parse_date_stage(ordinal_week_extended, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
get_parse_date_stage(ordinal_week_extended, parser) ->  {date, {week_extended, [{year,2013}, {weeknumber,29}]}};
get_parse_date_stage(ordinal_week_extended, value)  ->  {2013,7,15};

% Weekday
get_parse_date_stage(ordinal_weekday, lexer)  ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {decimal,"7",7}];
get_parse_date_stage(ordinal_weekday, parser) ->  {date, {weekday, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
get_parse_date_stage(ordinal_weekday, value)  ->  {2013,7,21};
% Weekday extended
get_parse_date_stage(ordinal_weekday_extended, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(ordinal_weekday_extended, parser)  ->  {date, {weekday_extended, [{year,2009}, {weeknumber,53}, {weekday,1}]}};
get_parse_date_stage(ordinal_weekday_extended, value) ->  {2009,12,28};




% Others
get_parse_date_stage(16, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(16, parser)  ->  {date, {ordinal, [{year,2013},{yearday,1}]}};
get_parse_date_stage(16, value) ->  {2013,1,1};
get_parse_date_stage(17, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(17, parser)  ->  {date, {ordinal_extended, [{year,2013},{yearday,1}]}};
get_parse_date_stage(17, value) ->  {2013,1,1};
get_parse_date_stage(19, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"7",7}];
get_parse_date_stage(19, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,29}, {weekday,7}]}};
get_parse_date_stage(19, value) ->  {2013,7,21};
get_parse_date_stage(20, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {decimal,"1",1}];
get_parse_date_stage(20, parser)  ->  {date, {weekday, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
get_parse_date_stage(20, value) ->  {2013,1,7};
get_parse_date_stage(21, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(21, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,2}, {weekday,1}]}};
get_parse_date_stage(21, value) ->  {2013,1,7};
get_parse_date_stage(22, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"2",2}, {decimal,"9",9}];
get_parse_date_stage(22, parser)  ->  {date, {week, [{year,2013}, {weeknumber,29}]}};
get_parse_date_stage(22, value) ->  {2013,7,15};
get_parse_date_stage(24, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(24, parser)  ->  {date, {week, [{year,2013}, {weeknumber,2}]}};
get_parse_date_stage(24, value) ->  {2013,1,7};
get_parse_date_stage(25, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"0",0}, {decimal,"2",2}];
get_parse_date_stage(25, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,2}]}};
get_parse_date_stage(25, value) ->  {2013,1,7};
get_parse_date_stage(52, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(52, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,1}]}};
get_parse_date_stage(52, value) ->  {2013,1,1};
get_parse_date_stage(53, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(53, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,1}]}};
get_parse_date_stage(53, value) ->  {2013,2,1};
get_parse_date_stage(54, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(54, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,1}]}};
get_parse_date_stage(54, value) ->  {2013,3,1};
get_parse_date_stage(55, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(55, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,1}]}};
get_parse_date_stage(55, value) ->  {2013,4,1};
get_parse_date_stage(56, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(56, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,1}]}};
get_parse_date_stage(56, value) ->  {2013,5,1};
get_parse_date_stage(57, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"6",6}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(57, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,6}, {monthday,1}]}};
get_parse_date_stage(57, value) ->  {2013,6,1};
get_parse_date_stage(58, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(58, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,1}]}};
get_parse_date_stage(58, value) ->  {2013,7,1};
get_parse_date_stage(59, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(59, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,1}]}};
get_parse_date_stage(59, value) ->  {2013,8,1};
get_parse_date_stage(60, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(60, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,1}]}};
get_parse_date_stage(60, value) ->  {2013,9,1};
get_parse_date_stage(61, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(61, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,1}]}};
get_parse_date_stage(61, value) ->  {2013,10,1};
get_parse_date_stage(62, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(62, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,1}]}};
get_parse_date_stage(62, value) ->  {2013,11,1};
get_parse_date_stage(63, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(63, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,1}]}};
get_parse_date_stage(63, value) ->  {2013,12,1};
get_parse_date_stage(64, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(64, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,1}, {monthday,31}]}};
get_parse_date_stage(64, value) ->  {2013,1,31};
get_parse_date_stage(65, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
get_parse_date_stage(65, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,2}, {monthday,28}]}};
get_parse_date_stage(65, value) ->  {2013,2,28};
get_parse_date_stage(66, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(66, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,3}, {monthday,31}]}};
get_parse_date_stage(66, value) ->  {2013,3,31};
get_parse_date_stage(67, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"4",4}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
get_parse_date_stage(67, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,4}, {monthday,30}]}};
get_parse_date_stage(67, value) ->  {2013,4,30};
get_parse_date_stage(68, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"5",5}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(68, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,5}, {monthday,31}]}};
get_parse_date_stage(68, value) ->  {2013,5,31};
get_parse_date_stage(70, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"7",7}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(70, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,7}, {monthday,31}]}};
get_parse_date_stage(70, value) ->  {2013,7,31};
get_parse_date_stage(71, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"8",8}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(71, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,8}, {monthday,31}]}};
get_parse_date_stage(71, value) ->  {2013,8,31};
get_parse_date_stage(72, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"9",9}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
get_parse_date_stage(72, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,9}, {monthday,30}]}};
get_parse_date_stage(72, value) ->  {2013,9,30};
get_parse_date_stage(73, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(73, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,10}, {monthday,31}]}};
get_parse_date_stage(73, value) ->  {2013,10,31};
get_parse_date_stage(74, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"0",0}];
get_parse_date_stage(74, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,11}, {monthday,30}]}};
get_parse_date_stage(74, value) ->  {2013,11,30};
get_parse_date_stage(75, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"1",1}];
get_parse_date_stage(75, parser)  ->  {date, {calendar_extended, [{year,2013}, {month,12}, {monthday,31}]}};
get_parse_date_stage(75, value) ->  {2013,12,31};
get_parse_date_stage(77, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"0",0}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(77, parser)  ->  {date, {weekday_extended, [{year,2010}, {weeknumber,52}, {weekday,1}]}};
get_parse_date_stage(77, value) ->  {2010,12,27};
get_parse_date_stage(78, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(78, parser)  ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,1}]}}; get_parse_date_stage(79, parser) ->  {date, {weekday_extended, [{year,2011}, {weeknumber,52}, {weekday,7}]}};
get_parse_date_stage(78, value) ->  {2011,12,26};
get_parse_date_stage(79, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"7",7}];
get_parse_date_stage(79, value) ->  {2012,1,1};
get_parse_date_stage(80, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(80, parser)  ->  {date, {weekday_extended, [{year,2012}, {weeknumber,52}, {weekday,1}]}};
get_parse_date_stage(80, value) ->  {2012,12,24};
get_parse_date_stage(81, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"1",1}];
get_parse_date_stage(81, parser)  ->  {date, {weekday_extended, [{year,2013}, {weeknumber,52}, {weekday,1}]}};
get_parse_date_stage(81, value) ->  {2013,12,23};
get_parse_date_stage(82, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_parse_date_stage(82, parser)  ->  {date, {week_extended, [{year,2012}, {weeknumber,52}]}};
get_parse_date_stage(82, value) ->  {2012,12,24};
get_parse_date_stage(83, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {minus,"-","-"}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_parse_date_stage(83, parser)  ->  {date, {week_extended, [{year,2013}, {weeknumber,52}]}};
get_parse_date_stage(83, value) ->  {2013,12,23};
get_parse_date_stage(85, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"3",3}, {week_separator,"W","W"}, {decimal,"5",5}, {decimal,"2",2}];
get_parse_date_stage(85, parser)  ->  {date, {week, [{year,2013}, {weeknumber,52}]}};
get_parse_date_stage(85, value) ->  {2013,12,23};
get_parse_date_stage(86, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(86, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,1}]}};
get_parse_date_stage(86, value) ->  {2012,2,1};
get_parse_date_stage(87, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"9",9}];
get_parse_date_stage(87, parser)  ->  {date, {calendar_extended, [{year,2012}, {month,2}, {monthday,29}]}};
get_parse_date_stage(87, value) ->  {2012,2,29};
get_parse_date_stage(88, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"6",6}];
get_parse_date_stage(88, parser)  ->  {date, {ordinal_extended, [{year,2012}, {yearday,366}]}};
get_parse_date_stage(88, value) ->  {2012,12,31};
get_parse_date_stage(89, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"1",1}];
get_parse_date_stage(89, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,1}]}};
get_parse_date_stage(89, value) ->  {2011,2,1};
get_parse_date_stage(90, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"0",0}, {decimal,"2",2}, {minus,"-","-"}, {decimal,"2",2}, {decimal,"8",8}];
get_parse_date_stage(90, parser)  ->  {date, {calendar_extended, [{year,2011}, {month,2}, {monthday,28}]}};
get_parse_date_stage(90, value) ->  {2011,2,28};
get_parse_date_stage(91, lexer) ->  [{decimal,"2",2}, {decimal,"0",0}, {decimal,"1",1}, {decimal,"1",1}, {minus,"-","-"}, {decimal,"3",3}, {decimal,"6",6}, {decimal,"5",5}];
get_parse_date_stage(91, parser)  ->  {date, {ordinal_extended, [{year,2011}, {yearday,365}]}};
get_parse_date_stage(91, value) ->  {2011,12,31};
get_parse_date_stage(Test, Stage) ->  throw({'Unknonwn test/stage',Test,Stage}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%% Internal functions



%% vim: set ts=2 sw=2 ai invlist si cul nu:
