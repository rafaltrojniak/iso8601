# About

This library parse, and format date in formats specified in ISO8601.
Currently it handles :
- Date format
- Time format
- Timezone format
- Combinations of above

Code quality is checked by unit tests, and documentation delivered using edoc.

All function calls allow to preserve format of data, and check if input is in expected format.

Parsing date and time formats is very complicated, and may depend on length of data.
After tries to parse data by positional comparision in erlang functions, I've decided to use yeec for that purpose.

# Simple usage
<pre><code>
2> iso8601:format_date({2013,2,3}).
"2013-02-03"
3> iso8601:parse_date("2013-02-03").
{{2013,2,3},calendar_extended}
4> iso8601:format_time({12,30,3},0).
"T123003"
5> iso8601:parse_time("T123003").
{{{12,30,3},0},general}
<BS>7> iso8601:parse_localtime("T123003+0100").
{{{12,30,3},0,3600},{general,minute}}
8> iso8601:format_datetime({2013,1,1},{12,30,3},0).
"20130101T123003"
9> iso8601:parse_datetime("20130101T123003").
{{{2013,1,1},{12,30,3},0},{calendar,general}}
</pre></code>


# TODO
- Parsing & formatting of Time intervals
- Parsing & formatting of Recurring time intervals
- Optimizations of lexer
