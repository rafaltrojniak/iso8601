Header "%% Copyright Rafał Trójniak <rafal@trojniak.net>".

Nonterminals date_format time_format format hour minute second monthday month year century weeknumber weekday yearday frac timezone_type timezone.

Terminals decimal time_designator time_separator frac_separator plus minus timezone_utc week_separator.

Rootsymbol format.

hour -> decimal decimal
	: {hour, decimal([element(3,'$1'),element(3,'$2')])}.
minute -> decimal decimal 
	: {minute, decimal([element(3,'$1'),element(3,'$2')])}.
second -> decimal decimal
	: {second, decimal([element(3,'$1'),element(3,'$2')])}.
frac -> decimal
	: element(2,'$1').
frac -> decimal frac
	: element(2,'$1') ++ '$2' .

timezone -> timezone_utc
	: {timezone,add,[]}.
timezone -> timezone_type hour
	: {timezone,'$1',['$2']}.
timezone -> timezone_type hour minute
	: {timezone,'$1',['$2','$3']}.
timezone -> timezone_type hour time_separator minute
	: {timezone,'$1',['$2','$4']}.

timezone_type  -> plus	: add.
timezone_type  -> minus	: sub.

time_format	->	 hour minute second 
	:{general,['$1','$2','$3']}.
time_format	->	 hour time_separator minute time_separator second 
	:{general_extended,['$1','$3','$5']}.
time_format	->	 hour minute 
	:{general_minute,['$1','$2']}.
time_format	->	 hour time_separator minute 
	:{general_minute_extended,['$1','$3']}.
time_format	->	 hour 
	:{general_hour,['$1']}.
time_format	->	 hour minute second frac_separator frac
	:{general_frac,['$1','$2','$3',{frac,1,'$5'}]}.
time_format	->	 hour time_separator minute time_separator second frac_separator frac
	:{general_extended_frac,['$1','$3','$5',{frac,1,'$7'}]}.
time_format	->	 hour minute frac_separator frac
	:{general_minute_frac,['$1','$2',{frac,60,'$4'}]}.
time_format	->	 hour time_separator minute frac_separator frac
	:{general_minute_extended_frac,['$1','$3',{frac,60,'$5'}]}.
time_format	->	 hour frac_separator frac
	:{general_hour_frac,['$1',{frac,3600,'$3'}]}.


monthday -> decimal decimal
	: {monthday, decimal([element(3,'$1'),element(3,'$2')])}.
month -> decimal decimal
	: {month, decimal([element(3,'$1'),element(3,'$2')])}.
year -> decimal decimal decimal decimal
	: {year, decimal([element(3,'$1'),element(3,'$2'),element(3,'$3'),element(3,'$4')])}.
century -> decimal decimal
	: {century, decimal([element(3,'$1'),element(3,'$2')])}.
weeknumber -> decimal decimal
	: {weeknumber, decimal([element(3,'$1'),element(3,'$2')])}.
weekday -> decimal
	: {weekday, element(3,'$1')}.
yearday -> decimal decimal decimal
	: {yearday, decimal([element(3,'$1'),element(3,'$2'),element(3,'$3')])}.

% Here month and monthday must be expanded to terminals, so threre will be no shift/reduce conflicts
date_format -> year decimal decimal decimal decimal
										% month monthday 
	: {calendar,['$1',
										{month, decimal([element(3,'$2'),element(3,'$3')])},
										{monthday, decimal([element(3,'$4'),element(3,'$5')])}
									 ]}.
date_format -> year decimal decimal decimal
							 			%yearday
	: {ordinal,['$1',
										{yearday, decimal([element(3,'$2'),element(3,'$3'),element(3,'$4')])}
									]}.

date_format -> year minus month minus monthday 
	: {calendar_extended,['$1','$3','$5']}.
date_format -> year minus month
	: {calendar_month,['$1','$3']}.
date_format -> year
	: {calendar_year,['$1']}.
date_format -> century
	: {calendar_century,['$1']}.
date_format -> year minus yearday
	: {ordinal_extended,['$1','$3']}.
date_format -> year week_separator weeknumber weekday
	: {weekday,['$1','$3','$4']}.
date_format -> year minus week_separator weeknumber minus weekday
	: {weekday_extended,['$1','$4','$6']}.
date_format -> year week_separator weeknumber
	: {week,['$1','$3']}.
date_format -> year minus week_separator weeknumber
	: {week_extended,['$1','$4']}.

format -> time_designator time_format
	: {time,'$2'}.
format -> time_designator time_format timezone
	: {localtime,'$2','$3'}.

format -> date_format
	: {date,'$1'}.
format -> date_format time_designator time_format
	: {datetime ,'$1','$3'}.
format -> date_format time_designator time_format timezone
	: {datetime_local ,'$1','$3','$4'}.


Erlang code.

decimal(X) ->
	decimal(lists:reverse(X),1).
decimal([],_) ->
	0;
decimal([Digit|X],Y) ->
 	Y*Digit + decimal(X,Y*10).
