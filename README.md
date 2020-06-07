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
:- demo_progress_bar.		% runs demo simple_progress_bar, default_progress_bar and fancy_progress_bar
:- demo_spinner.			% runs demo simple_spinner, default_spinner and fancy_spinner
```

## Documentation

See the pldoc documentation for additional information

## Files

```
progress_bar.pl - the progress_bar module
examples/demo.pl - a number of progress_bar and spinner examples
```

