:- module(print_table, [
    print_table/1, 
    print_table/2, 
    print_table/3, 
    print_table/4, 
    print_table/5,
    print_table/6
   ]).

/** <module> Formats and prints a table on the terminal screen 

The Data in the table is represented as a list of dicts, where each dict represents a Row. The Keys in the dict (i.e.
Row) correspond to a Column. The Value associated to a Key, represents the contents of a Cell. It is wrapped to fit the
Width of the Column (the wrapping alogyrithm alows a text to break at whitespace and hyphens, see wrap_text/3).

The formatting of the Table (i.e. Width and Height of Rows and Collumns) is automatically calculated. The heuristic used
is prioritize the columns with the most content to use the largest share of available width (i.e minimize the hight of a
row). If the content cannot be fit within the available width an Exception is raised. 

The formatting and rendering of the Table can be adapted in a number of ways:
 * Use a subset of available Columns, and define the order in which they are presented
 * Define the formatting of an individual Column (e.g width, alignmet, header, ...)
 * The Caption presented with the Table
 * The visual style of the Table (currently sypported, default, unicode, mysql, github)
 * The maximum Width the table can use.

print_table/6 allow the user to set all parrameters. The print_table/N with fewer arguments implement defaults. 
 
# Example

==
:- Data = [_{a:11,b:0.001,c:13},_{a:21,b:2.12,c:23},_{a:31,b:12.1111,c:33}],
print_table(Data,[b,c,a],_{b:_{align:right,format:"~2f"}},"Table",mysql,30).

%       Table (3 records) 
%      +-------+----+----+
%      |     b | c  | a  |
%      +-------+----+----+
%      |  0.00 | 13 | 11 |
%      |  2.12 | 23 | 21 |
%      | 12.11 | 33 | 31 |
%      +-------+----+----+
==

@author Joost Geurts
@license MIT License    
*/

:- use_module(library(wrap_text)).

:- use_module(library(clpfd)).
:- use_module(library(debug)).
%:- debug(print_table_style).
%:- debug(print_table_format).
%:- debug(print_table_label).
% :- debug(print_table_wrap).
% :- debug(print_table_size).


cell_template(cell{key:_, classes:[],style:default,format:"",content:"",column:_,row:_,
    width:_,height:_,cwidth:_,cheight:_,
    align:left, padding_left:0, padding_right:0,
    border_left:"",border_right:"",border_top:"",border_bottom:"",
    border_top_left:"",border_top_right:"",border_bottom_left:"",border_bottom_right:""}).

cell_style(default, stylesheet{
    header :        _{align:center,border_bottom:"-",border_bottom_right:" "},
    first_row :     _{},
    last_row :      _{},
    first_column :  _{},
    last_column :   _{},
    default :       _{padding_left:1, border_right:" ",padding_right:1}
    }).

cell_style(mysql, stylesheet{
    header :        _{align:center,border_top:"-",border_bottom:"-",border_top_left:"+",border_bottom_left:"+"},
    first_row :     _{},
    last_row :      _{border_bottom:"-",border_bottom_left:"+"},
    first_column :  _{},
    last_column :   _{border_right:"|",border_top_right:"+",border_bottom_right:"+"},
    default :       _{border_left:"|",padding_left:1, padding_right:1}
    }).

cell_style(unicode, stylesheet{
    header :        _{align:center,border_top:"─",border_top_left:"┌"},
    first_row :     _{border_top:""},
    last_row :      _{border_bottom:"─",border_bottom_left:"┴"},
    first_column :  _{border_bottom_left:"└"},
    last_column :   _{border_right:"│",border_top_right:"┐",border_bottom_right:"┘"},
    default :       _{border_bottom:"─",border_left:"│",border_bottom_left:"┼",padding_left:1, padding_right:1}
    }).

cell_style(github, stylesheet{
    header :        _{align:center,border_left:"|",border_bottom_left:"|",border_bottom:"-"},
    first_row :     _{},
    last_row :      _{},
    first_column :  _{},
    last_column :   _{border_right:"|"},
    default :       _{border_left:"|",padding_left:1, padding_right:1}
    }).


%%  print_table(+Data:list(dict)) is det.
%%  print_table(+Data:list(dict), +Keys:list(atom)) is det.
%%  print_table(+Data:list(dict), +Keys:list(atom), +ColumnsSpec:dict) is det.
%%  print_table(+Data:list(dict), +Keys:list(atom), +ColumnsSpec:dict, +Caption:string) is det.
%%  print_table(+Data:list(dict), +Keys:list(atom), +ColumnsSpec:dict, +Caption:string, +Style:atom) is det.
%%  print_table(+Data:list(dict), +Keys:list(atom), +ColumnsSpec:dict, +Caption:string, +Style:atom, +MaxWidth:integer) is det.
%    * =Data=
%    is a list of Dicts, where each Dict represent a Row of the Table. The union of all Keys in the Rows reprent the Columns of the table (if a Dict doesn't define a Key it is filled with 'null')
%    * =Keys=
%    selects the Columns that will be printed, as well a their order.   
%    * =ColumnsSpec=
%    allows the user to specify the formating of a particular column (see below) 
%    It is a is a Dict, whose Keys reference a Column. The Value is a Dict recognizes the folowing Keys: 
%       * =header=
%       to define the text that is printed as header (by default the Key is used)
%       * =width=
%       to define the width of the column (by default this is calculated automatically)
%       * =format=
%       to define the formatting template (see format/2) used to print the value (by default "~w" is used)
%       * =align=
%       sets the alignment within the cell (left, center, right)  
%    *  =Caption=
%    Sets the Caption of the Table   
%    *  =Style= 
%    sets the visual style to print the table. current options are =default=, =unicode=, =mysql=, =github=  
%    * =MaxWidth=
%    defines the Maxium width (in characters) the Table can have. By default the whole terminal width is used
%   
%    

print_table(Data) :-
    dicts_same_keys(Data,Keys),
    print_table(Data,Keys).

print_table(Data,Keys) :-
    ColumnsSpec = _{},
    print_table(Data,Keys,ColumnsSpec).

print_table(Data,Keys,ColumnsSpec) :-
    Caption = "Table ",
    print_table(Data,Keys,ColumnsSpec,Caption).

print_table(Data,Keys,ColumnsSpec,Caption) :-
    Style = default,
    print_table(Data,Keys,ColumnsSpec,Caption,Style).

print_table(Data,Keys,ColumnsSpec,Caption,Style) :-
    tty_size(_,TerminalWidth),
    MaxWidth is TerminalWidth - 2,
    print_table(Data,Keys,ColumnsSpec,Caption,Style,MaxWidth).

print_table(Data,Keys,ColumnsSpec,Caption,Style,MaxWidth) :-
    must_be(list(dict),Data),
    must_be(list(atom),Keys),
    must_be(dict,ColumnsSpec),
    must_be(positive_integer,MaxWidth),
    normalise_table(Keys,Data,NKeys,NData),
    catch_with_backtrace(
        format_table(NKeys,NData,Style,ColumnsSpec,MaxWidth,Table),Error,print_message(error, Error)),
    print_message(informational,print_table(table(Table,Caption,MaxWidth))).

%%  normalise_table(Keys,Table,NTable)
%   ensures Keys and Data are defined
normalise_table(Keys,Data,NKeys,NData) :-
    dicts_slice(Keys,Data,SlicedData),
    dicts_to_same_keys(SlicedData,dict_fill(null),NData),
    dicts_same_keys(NData,KeysPresent),
    findall(Key,(member(Key,Keys),member(Key,KeysPresent)),NKeys).

%%  format_table(+Keys:list(atom), +Data:list(dict), +Style:atom, +ColumnsSpec:dict, +MaxWidth:int,-Table:list(list(dict))) is det.
%   Formats a Table within the given space constraints defined by MaxWidth
format_table(Keys,Data,Style,ColumnsSpec,MaxWidth,Table) :-
    pre_process_table(Keys,Data,ColumnsSpec,Table0),
    apply_style(Style,ColumnsSpec,Table0,Table1),
    size_columns(Table1,MaxWidth,Table),
    forall(member(Row,Table),
        (
            findall(CellSpec,(member(Cell,Row),format(atom(CellSpec),"(~w,~w)",[Cell.width,Cell.height])),CellSpecs),
            debug(print_table_size,"~w",[CellSpecs])
        )).

%%  size_columns(+Rows,+MaxWidth,-NRows) is det.
%   For each column selects the cells that are most excessive in terms of necessary resources
%   Calculate the necessary width for each column (whilte not exceeding MaxWidth) to satistfy these requirements, 
%   noting that the less excesive cells will naturually fit.
%   @throws goal_failed If Rows cannot be fitted within MaxWidth x MaxHeight
size_columns(Rows,MaxWidth,NRows) :-
    transpose(Rows,Columns),
    compute_column_widths(Columns,ColSpecs),
    Height in 0..10000,
    set_equal_value(height,ColSpecs,Height),
    cell_widths(ColSpecs,CellWidths),
    sum(CellWidths,#=,TableWidth),
    TableWidth #=< MaxWidth,
    label([Height]),
    label_cells(ColSpecs),
    set_column_widths(Columns,ColSpecs,NColumns),
    transpose(NColumns,NRows),!.

size_columns(Rows,MaxWidth,_) :-
    throw(error(insufficient_width(Rows,MaxWidth),_)).

compute_column_widths([],[]).
compute_column_widths([Column|Columns],[ColSpec|ColSpecs]) :-
    compute_column_width(Column,ColSpec),
    compute_column_widths(Columns,ColSpecs).
    
compute_column_width(Column,_{key:Key,width:Width,height:Height}) :-
    Column = [C0|_],
    Width = C0.width,
    % findall(W,(member(C0,Column),W = C0.width, number(Width)),Ws),max_list(Ws,Width0),
    % (nonvar(Width0) -> Width = Width0 ; Width = _),
    sort(length,@>=,Column,[C1|_]), MaxLength = C1.length, Key = C1.key,
    sort(min_width,@>=,Column,[C2|_]), MinWidth = C2.min_width,
    sort(padding_left,@>=,Column,[C3|_]), MaxPL = C3.padding_left,
    sort(padding_right,@>=,Column,[C4|_]), MaxPR = C4.padding_right,
    findall(BLL,(member(C,Column),string_length(C.border_left,BLL)),BLLs),max_list(BLLs,MaxBL),
    findall(BRL,(member(C,Column),string_length(C.border_right,BRL)),BRLs),max_list(BRLs,MaxBR),
    Width #>= 0,
    CWidth #>= 0,
    Width #= MaxBL + MaxPL + CWidth + MaxPR + MaxBR,
    Height #>= 0,
    CWidth #>= MinWidth,
    CWidth * Height #>= MaxLength.

set_column_widths([],_,[]).
set_column_widths([Column|Columns],ColSpecs,[NColumn|NColumns]) :-
    set_cells_widths(Column,ColSpecs,NColumn),
    set_column_widths(Columns,ColSpecs,NColumns).

set_cells_widths([],_,[]).
set_cells_widths([Cell|Cells],ColSpecs,[NCell|NCells]) :-
    member(ColSpec,ColSpecs),
    ColSpec.key = Cell.key,
    set_cell_width(Cell,ColSpec,NCell),
    set_cells_widths(Cells,ColSpecs,NCells).

set_cell_width(Cell,ColSpec,NCell) :-
    Width = ColSpec.width,
    string_length(Cell.border_left,BL),
    string_length(Cell.border_right,BR),
    CWidth is Width - BL - Cell.padding_left - Cell.padding_right - BR,
    NCell = Cell.put(_{width:Width,cwidth:CWidth}).

%% set_equal_value(+Att,+Cells,+Value)
%  sets the attribute values denoted by Att within Cells to be equal Value
set_equal_value(_,[],_).
set_equal_value(Att,[Cell|Cells],Value) :-
    Cell.Att #= Value,
    set_equal_value(Att,Cells,Value).

cell_widths([],[]).
cell_widths([Cell|Cells],[Width|Widths]) :-
    Width = Cell.width,
    cell_widths(Cells,Widths).

%%  label_cells(+Cells) is det.
%   assigns a width and height to a cell (if there are options)
label_cells([]).
label_cells([Cell|Cells]) :-
    fd_inf(Cell.width,WidthInf),fd_sup(Cell.width,WidthSup),
    fd_inf(Cell.height,HeightInf),fd_sup(Cell.height,HeightSup),
    label([Cell.height,Cell.width]),
    DeltaWidth is Cell.width - WidthInf,
    DeltaHeight is Cell.height - HeightInf,
    debug(print_table_label,"label cell - width(~w .. ~w): ~w (+~w), height(~w .. ~w): ~w (+~w)",
        [WidthInf,WidthSup,Cell.width,DeltaWidth,HeightInf,HeightSup,Cell.height,DeltaHeight]),
    label_cells(Cells).
%% pre_process_table(+Keys:list(atom),+Data:list(dict), +ColumnsSpec:dict,-Table:list(list(dict))) is det.
%   creates an internal Table structure composed of Rows of Cells
pre_process_table(Keys,Data,ColumnsSpec,Table) :-
    create_header(Keys,ColumnsSpec,Header),
    table_to_cell_rows(Keys,[Header|Data],ColumnsSpec,TableData0),
    pre_process_rows(TableData0,TableRows),
    transpose(TableRows,TableColumns0),
    pre_process_columns(TableColumns0,TableColumns),
    transpose(TableColumns,Table),!.

pre_process_rows(Rows,NRows) :-
    length(Rows,N), N = 2,!,
    Rows = [HeaderRow0,FirstRow0],
    set_class(header,HeaderRow0,HeaderRow),
    set_class(first_row,FirstRow0,FirstRow1),   %  FirstRow = first & last row
    set_class(last_row,FirstRow1,FirstRow),
    NRows = [HeaderRow,FirstRow].

pre_process_rows(Rows,NRows) :-
    append([HeaderRow0,FirstRow0|RemainingRows],[LastRow0],Rows),  
    set_class(header,HeaderRow0,HeaderRow),
    set_class(first_row,FirstRow0,FirstRow),
    set_class(last_row,LastRow0,LastRow),
    append([HeaderRow,FirstRow|RemainingRows],[LastRow],NRows).

pre_process_columns([Column0],[NColumn]) :-
    !,
    set_class(first_column,Column0,Column1),    %  Column = first & last column
    set_class(last_column,Column1,NColumn).

pre_process_columns(Columns,NColumns) :-
    append([FirstColumn0|RemainingColumns],[LastColumn0],Columns), 
    set_class(first_column,FirstColumn0,FirstColumn),
    set_class(last_column,LastColumn0,LastColumn),
    append([FirstColumn|RemainingColumns],[LastColumn],NColumns).

% creates a default representation for the Header (that can be manipulated later using ColumnSpec)
create_header(Keys,ColumnsSpec,Header) :-
    findall(Key:Value,
        (
            member(Key,Keys),
            (Value = ColumnsSpec.get(Key).get(header) -> true ; Value = Key)
        ), HeaderData),
    dict_create(Header,_,HeaderData).

%%  table_to_cell_rows(+Keys:list(atom), +TableData:list(dict), +ColumnSpecs:dict, -NTable:list(list(dict))) is det.
%   converts the atomic values in a row (represented as dict) to a list of lists containing dict structure representing a table cell
table_to_cell_rows(Keys,TableData,ColumnsSpec,NTable) :-
    cell_template(CellTemplate0),
    findall(NRow,
        (
            member(Row,TableData),
            nth0(RowIndex,TableData,Row),
            findall(Cell,
                (
                    member(Key,Keys),
                    nth0(ColumnIndex,Keys,Key),
                    (UserTemplate = ColumnsSpec.get(Key) -> CellTemplate = CellTemplate0.put(UserTemplate) ; CellTemplate = CellTemplate0),
                    Value = Row.get(Key),
                    (RowIndex = 0 -> FormatTemplate = "~w" ; FormatTemplate = CellTemplate.format),
                    format_content(Value,FormatTemplate,FormattedValue),
                    string_length(FormattedValue,Length),
                    min_wrap_width(FormattedValue,MinWidth),
                    Cell = CellTemplate.put(_{key:Key,content:Value,format:FormatTemplate,formatted_content:FormattedValue,length:Length,min_width:MinWidth,row:RowIndex,column:ColumnIndex})
                ),NRow)
        ),NTable).
 
set_class(_,[],[]) :- !.   
set_class(Class,[Cell|Cells],[NCell|NCells]) :-
    append(Cell.classes,[Class],NClasses),
    NCell = Cell.put(classes,NClasses),
    set_class(Class,Cells,NCells).

apply_style(_,_,[],[]) :- !.
apply_style(Style,ColumnsSpec,[Row|Rows],[NRow|NRows]) :-
    cell_style(Style, StyleSheet),
    findall(NCell,(
        member(Cell0,Row),
        (StyleObj = StyleSheet.get(default) -> true ; StyleObj = _{}),
        Cell1 = Cell0.put(StyleObj),
        apply_style_to_cell(StyleSheet,Cell1.classes,Cell1,Cell),
        ((Cell1.row > 0,ColumnSpec = ColumnsSpec.get(Cell.key)) -> true ; ColumnSpec = _{}),
        NCell = Cell.put(ColumnSpec)
        ),NRow),
    apply_style(Style,ColumnsSpec,Rows,NRows).

apply_style_to_cell(_,[],NCell,NCell).
apply_style_to_cell(StyleSheet,[Class|Classes],Cell0,NCell) :-
    (StyleObj = StyleSheet.get(Class) -> true ; StyleObj = _{}),
    Cell = Cell0.put(StyleObj),
    debug(print_table_style,"apply style - content:~w, class:~w, style:~w",[Cell.content, Class, StyleObj]),
    apply_style_to_cell(StyleSheet,Classes,Cell,NCell).

%% create_row_template(+Row,+StartPosition,-Template)
create_row_template(StartPosition,Row,Template) :-
    % Rs = [_{padding_left:1,padding_right:1,border_left:"*",border_right:"*",width:12},_{padding_left:1,padding_right:1,border_left:"*",border_right:"*",width:12}],create_row_template(0,Rs,T),format(T,[xxxxxx,yyyyyy]).
    format(string(Start),"~~~w|",[StartPosition]),
    create_row_template2(Row,Start,Template).

create_row_template2([],Template,Template).
create_row_template2([Cell|Cells],Acc,Template) :-
    create_cell_template(Cell,CellTemplate),
    string_concat(Acc,CellTemplate,NAcc),
    create_row_template2(Cells,NAcc,Template).

create_cell_template(Cell,Template) :-
    (Cell.align = left -> SubTemplate = "~~w~~t~~~w+" ;
        (Cell.align = right -> SubTemplate = "~~t~~w~~~w+" ;
            SubTemplate = "~~t~~w~~t~~~w+")), % center
    format(string(ContentTemplate),SubTemplate,[Cell.cwidth]),
    format(string(Template),"~w~~|~~t~~~w+~w~~t~~~w+~w", [Cell.border_left,Cell.padding_left,ContentTemplate,Cell.padding_right,Cell.border_right]),
    debug(print_table_format,"~w~30|content: ~w,~70|border_left:~w, padding_left:~w, cwidth:~w, cheight:~w, padding_right:~w, border_right:~w",
        [Template,Cell.content, Cell.border_left,Cell.padding_left,Cell.cwidth,Cell.cheight,Cell.padding_right,Cell.border_right]).

create_border_template(StartPosition,Cells,Att,Str) :-
    % Rs = [_{border_top:"*",border_bottom:"*",width:12},_{border_top:"*",border_bottom:"*",width:12}],create_border_template(0,Rs,border_top,T),format(T).
    format(string(Start),"~~~w|",[StartPosition]),
    create_border_template2(Cells,Att,Start,Str).

create_border_template2([],_,Template,Template).
create_border_template2([Cell|Cells],Att,Acc,Template) :-
    atom_concat(Att,'_left',CornerLeft),atom_concat(Att,'_right',CornerRight),
    (string_length(Cell.Att,1) -> BorderChar = Cell.Att ; BorderChar = " "),
    (string_length(Cell.CornerLeft,1) -> CornerLeftChar = Cell.CornerLeft ; CornerLeftChar = BorderChar),
    (string_length(Cell.CornerRight,1) -> CornerRightChar = Cell.CornerRight ; CornerRightChar = BorderChar),
    % format(string(CellTemplate),"~~|~~`~wt~~~w+",[BorderChar,Cell.width]),
    format(string(CellTemplate),"~~|~w~~`~wt~w~~~w+",[CornerLeftChar,BorderChar,CornerRightChar,Cell.width]),
    string_concat(Acc,CellTemplate,NAcc),
    create_border_template2(Cells,Att,NAcc,Template).

%% pop_line(+Cells:list,-FirstLines:list, -RemainingRowLines:list) is det.
%   returns the first lines, and the remaining lines
pop_line([],[],[]).
pop_line([Cell|Cells],[FirstLine|RowLines],[Rest|RemainingRowLines]) :-
    Cell = [FirstLine|Rest],!,
    pop_line(Cells,RowLines,RemainingRowLines).

pop_line([Cell|Cells],[""|RowLines],[[]|RemainingRowLines]) :-
    Cell = [],
    pop_line(Cells,RowLines,RemainingRowLines).

format_content(Content,FormatTemplate,Str) :-
    phrase(content(Content,FormatTemplate),Codes),
    string_codes(Str,Codes).

         /*******************************
         *       MESSAGES               *
         *******************************/

:- multifile prolog:message//1.
:- multifile prolog:error_message//1.

prolog:message(print_table(Msg)) --> Msg.

table(Rows,Caption,Width) --> {
    Rows = [Row|_],findall(CellWidth,(member(Cell,Row),CellWidth = Cell.width),Widths),
    sum_list(Widths,TableWidth),
    StartPos is floor((Width - TableWidth) / 2),
    length(Rows,N), NResults is N - 1
    },
    caption(Caption,StartPos,TableWidth,NResults),rows(Rows,StartPos).

caption(Mesg,StartPos,TableWidth,NResults) --> {
    format(string(Template),"~~~w|~~t~~w (~w records)~~t~~~w+",[StartPos,NResults,TableWidth]),
    format(string(Caption),Template,[Mesg])
    },
    [Caption,nl].

rows([],_) --> !.
rows([Row|Rows],StartPos) --> row(Row,StartPos),rows(Rows,StartPos).

row(Row,StartPos) --> border_line(Row,border_top,StartPos),row_content(Row,StartPos),border_line(Row,border_bottom,StartPos).

border_line([],_,_) --> !.
border_line(Row,Att,_) --> {forall(member(Cell,Row), Cell.Att = "")},!.
border_line(Row,Att,StartPos) --> {
    create_border_template(StartPos,Row,Att,Template),
    format(string(BorderLine),Template,[])
    },
    [BorderLine,nl].

row_content(Row,StartPos) --> {
    create_row_template(StartPos,Row,RowTemplate),
    findall(WrappedLines, (
        member(Cell,Row),
        Content = Cell.formatted_content,
        wrap_text(Cell.cwidth,Content,WrappedLines),
        debug(print_table_wrap,"wrap_text: width:~w, content:~w, wrapped:~w",[Cell.width,Content,WrappedLines])
        ), RowLines)
    },
    row_lines(RowLines,RowTemplate).

row_lines(RowLines,_) --> {forall(member(Lines,RowLines),Lines = [])},!.
row_lines(RowLines,Template) --> {
    pop_line(RowLines,FirstLines,Remaining),
    format(string(RowStr),Template,FirstLines)
    },[RowStr,nl],row_lines(Remaining,Template).

% defaults
content(Content) --> {Content = date(_,_,_),!, format_time(string(DStr),"%Y-%m-%d",Content)},content(DStr,"~w").
content(Content) --> {number(Content),!},content(Content,"~d").
content(Content) --> content(Content,"~w").

content(Content,"") --> !,content(Content).
content(Content,Template) --> {format(string(Str),Template,[Content])},Str.

prolog:error_message(insufficient_width(_,Width)) -->
    [ 'Insufficient width to print table (provided: ~w)'-[Width] ].

