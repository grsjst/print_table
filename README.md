# Formats and prints a table on the terminal screen (print_table)

print_table is a Pack for SWI-Prolog (https://www.swi-prolog.org/) that formats and prints a Table on the terminal sreen
. 

These are few examples: 
```swipl
?- Data = [_{a:11,b:0.001,c:13},_{a:21,b:2.12,c:23},_{a:31,b:12.1111,c:33}],                                                             print_table(Data,[b,c,a],_{b:_{align:right,format:"~2f"}},"Table",mysql,30).

%       Table (3 records) 
%      +-------+----+----+
%      |   b   | c  | a  |
%      +-------+----+----+
%      |  0.00 | 13 | 11 |
%      |  2.12 | 23 | 21 |
%      | 12.11 | 33 | 31 |
%      +-------+----+----+
```

```swipl
?- Data = [_{bla:"bla, bla, bla",duh:"duh!",ehm:"ehm..."},_{bla:"bladibla",duh:"di di duh",ehm:"ehmmmm ehhhhmmmm"}],
			print_table(Data,[ehm,bla,duh],_{},"Table",unicode,40).
%            Table (2 records)        
%     ┌───────────┌──────────┌───────┐
%     │    ehm    │   bla    │  duh  │
%     └───────────┼──────────┼───────┘
%     │ ehm...    │ bla,     │ duh!  │
%     │           │ bla, bla │       │
%     └───────────┼──────────┼───────┘
%     │ ehmmmm    │ bladibla │ di di │
%     │ ehhhhmmmm │          │ duh   │
%     └───────────┴──────────┴───────┘
```

## Installation

```swipl
pack_install(print_table).
```
## Usage

In your SWIPL programme include the directive: 

```swipl
:- use_module(print_table).
```

## Examples

In the file `examples\demo.pl` a number of demos are provided 

```swipl
:- ['./examples/demo.pl']. 	% loads the demo
:- test_table1(a).			% runs demo a
:- test_table1(b).			% runs demo b
```

## Documentation

See the pldoc documentation for additional information

## Files

```
prolog/print_table.pl 		- the print_table module
prolog/wrap_text.pl 		- wraps an arbitrary text into lines that do not exceed the specified length
examples/demo.pl 		- a number of examples
tests/test_wrap.pl 		- unit tests for the wrap_text module

```

## Related and Issues

Similar to `print_table`, the pack `clitable` (see https://www.swi-prolog.org/pack/list?p=clitable) allows for pretty printing a Table structure. However, it doesn't format the table within the available space. 

The performace is influenced by the number of rows in the table.  

