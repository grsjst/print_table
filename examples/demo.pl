:- use_module(library(print_table)).

test_table(a,[a,b,c],30,[row{a:"11 11",b:2,c:"xxxx xxx xx  x xxxxx"},row{a:1,b:"222 xxxz ddd dd ",c:3},row{a:1,b:2,c:333}]).
test_table(b,[a,b,c],57,[row{a:"001",b:"COrUSCENT",c:"he quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog."}]).
test_table(c,[a,b,c],30,[row{a:"11 11"}]).
test_table(d,[a,b,c],30,[row{a:"11 11"},row{a:"22 22"}]).

test_table1(Id) :-
    test_table(Id,Keys,MaxWidth,Data),
	print_table(Data,Keys,_{},"Test Table 1",mysql,MaxWidth).