Definitions.

Rules.

[0-9] : {token,{decimal,TokenChars, list_to_integer(TokenChars)}}.

T : {token,{time_designator,TokenChars,TokenChars}}.
, : {token,{frac_separator,TokenChars,TokenChars}}.
: : {token,{time_separator,TokenChars,TokenChars}}.
\+ : {token,{plus,TokenChars,TokenChars}}.
\- : {token,{minus,TokenChars,TokenChars}}.
W : {token,{week_separator,TokenChars,TokenChars}}.
Z : {token,{timezone_utc,TokenChars,TokenChars}}.


Erlang code.
