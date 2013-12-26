%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Testing date parsing library
%%% @end
%%% Created :  wto lip 02 23:34:20 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(iso8601_lexer_test).

-include_lib("eunit/include/eunit.hrl").

parse_lexer_test_() ->
    [
        % Separators and designators
        ?_assertEqual({ok,[{minus,"-","-"}],1},iso8601_lexer:string("-")),
        ?_assertEqual({ok,[{plus,"+","+"}],1},iso8601_lexer:string("+")),
        ?_assertEqual({ok,[{frac_separator,",",","}],1},iso8601_lexer:string(",")),
        ?_assertEqual({ok,[{time_separator,":",":"}],1},iso8601_lexer:string(":")),
        ?_assertEqual({ok,[{time_designator,"T","T"}],1},iso8601_lexer:string("T")),
        ?_assertEqual({ok,[{week_separator,"W","W"}],1},iso8601_lexer:string("W")),
        ?_assertEqual({ok,[{timezone_utc,"Z","Z"}],1},iso8601_lexer:string("Z")),
        % Decimals
        ?_assertEqual({ok,[{decimal,"0",0}],1},iso8601_lexer:string("0")),
        ?_assertEqual({ok,[{decimal,"1",1}],1},iso8601_lexer:string("1")),
        ?_assertEqual({ok,[{decimal,"2",2}],1},iso8601_lexer:string("2")),
        ?_assertEqual({ok,[{decimal,"3",3}],1},iso8601_lexer:string("3")),
        ?_assertEqual({ok,[{decimal,"4",4}],1},iso8601_lexer:string("4")),
        ?_assertEqual({ok,[{decimal,"5",5}],1},iso8601_lexer:string("5")),
        ?_assertEqual({ok,[{decimal,"6",6}],1},iso8601_lexer:string("6")),
        ?_assertEqual({ok,[{decimal,"7",7}],1},iso8601_lexer:string("7")),
        ?_assertEqual({ok,[{decimal,"8",8}],1},iso8601_lexer:string("8")),
        ?_assertEqual({ok,[{decimal,"9",9}],1},iso8601_lexer:string("9")),
        % Few errors
        ?_assertEqual({error, {1,iso8601_lexer,{illegal,"p"}} ,1 }, iso8601_lexer:string("p")),
        ?_assertEqual({error, {1,iso8601_lexer,{illegal,"x"}} ,1 }, iso8601_lexer:string("x")),
        % Few multi-token tests
        ?_assertEqual({ok,[
            {decimal,"0",0},
            {decimal,"1",1},
            {decimal,"2",2},
            {decimal,"3",3},
            {decimal,"4",4},
            {frac_separator,",",","},
            {time_separator,":",":"}],1},
                  iso8601_lexer:string("01234,:")),
        ?_assertEqual({error, {1,iso8601_lexer,{illegal,"p"}} ,1 }, iso8601_lexer:string("poc"))
    ].
