# About

This library parses, and frmats date in formats specified in ISO8601.

There are two ways to use the functions : with specification of the format, and autodetecting format.

# Simple usage
<pre><code>
1> iso8601:parse_date("2013-W02-1").
{2013,1,7}
2> iso8601:parse_date("2013-02-01").
{2013,2,1}
3> iso8601:parse_date("2013-101").
{2013,4,11}
4> iso8601:parse_date("2013-07-21", calendar_extended ).
{2013,7,21}
5> iso8601:format_date({2011,12,31}).
"2011-12-31"
6> iso8601:format_date({2011,12,31}, calendar).
"20111231"
7> iso8601:format_date({2011,12,31}, calendar_extended).
"2011-12-31"
8> iso8601:format_date({2011,12,31}, calendar_month).
"2011-12"
</pre></code>at_date({2011,12,31}, calendar_extended).
"2011-12-31"


# TODO
- Test parser & lexer
- Test date builderat_date({2011,12,31}, calendar_extended).
"2011-12-31"
