:- module(test_wrap_text, [
	test_wrap_text/0
	]).

:- use_module(library(plunit)).
:- use_module(library(wrap_text)).

test_wrap_text :-
	run_tests([wrap_text]).

:- begin_tests(wrap_text, []).

test(empty_string) :-  wrap_text(0,"",[]).

test(break_ff) :-  wrap_text(0,"\f",[""]).
test(break_cr) :-  wrap_text(0,"\r",[""]).
test(break_nl) :-  wrap_text(0,"\n",[""]).

test(break_nl_nl) :-  wrap_text(0,"\n\n",["",""]).

test(space) :-  wrap_text(1," ",[" "]).
test(spaces) :- wrap_text(2,"  ",["  "]).
test(tab) :- wrap_text(3,"\t",["   "]).

test(trailing_spaces1) :- wrap_text(2,"a ",["a "]).
test(trailing_spaces2) :- wrap_text(3,"a  ",["a  "]).
test(trailing_spaces3) :- wrap_text(4,"a   ",["a   "]).

test(one_char) :-  wrap_text(1,"a",["a"]).
test(one_word) :-  wrap_text(3,"aa",["aa"]).
test(words) :-  wrap_text(6,"aa  bb",["aa  bb"]).

test(break_space) :-  wrap_text(2,"a b",["a","b"]).
test(break_conserve_space) :-  wrap_text(2,"a  b",["a"," b"]).

test(break_nl) :-  wrap_text(2,"a\nb",["a","b"]).

test(break_hyphen) :-  wrap_text(2,"a-b",["a-","b"]).

test(single_hyphen) :- wrap_text(3," - ",[" - "]).

:- end_tests(wrap_text).