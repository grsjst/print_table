:- module(wrap_text, [
    wrap_text/3,
    min_wrap_width/2
   ]).

/** <module> wraps a given text in lines of specified length

    a line is represented as an atom in a list. It does not contain and end_of_line character at the end 

    supported break-points :
    * end_of_line character
    * whitespaces
    * hyphens

@author Joost Geurts
@license MIT License   
*/

:- use_module(library(dcg/basics)).

:- use_module(library(debug)).
:- debug(wrap_text).

%text(NWords:int,Text:string).

text(50, "The European languages are members\nof\nthe same family. Their separate existence is a myth. For science, music, sport, etc, Europe uses the same vocabu-lary. The languages only differ in their grammar, their pronunciation and their most common words. Everyone realizes why a new common language would be desirable: one").
text(100,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave").
text(200,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV for luck. A wizard’s job is to vex chumps quickly in fog. Watch \"Jeopardy!\", Alex Trebek's fun TV quiz game. Woven silk pyjamas exchanged for blue quartz. Brawny gods just").
text(250,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV for luck. A wizard’s job is to vex chumps quickly in fog. Watch \"Jeopardy!\", Alex Trebek's fun TV quiz game. Woven silk pyjamas exchanged for blue quartz. Brawny gods just flocked up to quiz and vex him. Adjusting quiver and bow, Zompyc[1] killed the fox. My faxed joke won a pager in the cable TV quiz show. Amazingly few discotheques provide jukeboxes. My girl wove six dozen plaid jackets before she quit. Six big devils from Japan quickly forgot how").
text(275,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV for luck. A wizard’s job is to vex chumps quickly in fog. Watch \"Jeopardy!\", Alex Trebek's fun TV quiz game. Woven silk pyjamas exchanged for blue quartz. Brawny gods just flocked up to quiz and vex him. Adjusting quiver and bow, Zompyc[1] killed the fox. My faxed joke won a pager in the cable TV quiz show. Amazingly few discotheques provide jukeboxes. My girl wove six dozen plaid jackets before she quit. Six big devils from Japan quickly forgot how to waltz. Big July earthquakes confound zany experimental vow. Foxy parsons quiz and cajole the lovably dim wiki-girl. Have a pick: twenty six letters").
text(300,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV for luck. A wizard’s job is to vex chumps quickly in fog. Watch \"Jeopardy!\", Alex Trebek's fun TV quiz game. Woven silk pyjamas exchanged for blue quartz. Brawny gods just flocked up to quiz and vex him. Adjusting quiver and bow, Zompyc[1] killed the fox. My faxed joke won a pager in the cable TV quiz show. Amazingly few discotheques provide jukeboxes. My girl wove six dozen plaid jackets before she quit. Six big devils from Japan quickly forgot how to waltz. Big July earthquakes confound zany experimental vow. Foxy parsons quiz and cajole the lovably dim wiki-girl. Have a pick: twenty six letters - no forcing a jumbled quiz! Crazy Fredericka bought many very exquisite opal jewels. Sixty zippers were quickly picked from the woven jute bag. A quick").
text(500,"The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV for luck. A wizard’s job is to vex chumps quickly in fog. Watch \"Jeopardy!\", Alex Trebek's fun TV quiz game. Woven silk pyjamas exchanged for blue quartz. Brawny gods just flocked up to quiz and vex him. Adjusting quiver and bow, Zompyc[1] killed the fox. My faxed joke won a pager in the cable TV quiz show. Amazingly few discotheques provide jukeboxes. My girl wove six dozen plaid jackets before she quit. Six big devils from Japan quickly forgot how to waltz. Big July earthquakes confound zany experimental vow. Foxy parsons quiz and cajole the lovably dim wiki-girl. Have a pick: twenty six letters - no forcing a jumbled quiz! Crazy Fredericka bought many very exquisite opal jewels. Sixty zippers were quickly picked from the woven jute bag. A quick movement of the enemy will jeopardize six gunboats. All questions asked by five watch experts amazed the judge. Jack quietly moved up front and seized the big ball of wax.The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz. Brick quiz whangs jumpy veldt fox. Bright vixens jump; dozy fowl quack. Quick wafting zephyrs vex bold Jim. Quick zephyrs blow, vexing daft Jim. Sex-charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! \"Now fax quiz Jack!\" my brave ghost pled. Five quacking zephyrs jolt my wax bed. Flummoxed by job, kvetching W. zaps Iraq. Cozy sphinx waves quart jug of bad milk. A very bad quack might jinx zippy fowls. Few quips galvanized the mock jury box. Quick brown dogs jump over the lazy fox. The jay, pig, fox, zebra, and my wolves quack! Blowzy red vixens fight for a quick jump. Joaquin Phoenix was gazed by MTV").
text(lorem,"Lorem īpsūm dolor sit amet, timēǽm æntīōpam ċonċlusionemque eu nam. Ubiqūe prīncipes ƿec ēx, delectus moderatius deliċātissimī cum æð! Eum āffert putenÞ et, ne hās cetēros vīvendō vūlputaÞe. Enīm plætoƿem nǽm eÞ, ēū grǽece dicunt mel. Ei ālterǣ ancillæe his, ei ēōs lāboræmūs dispuÞāndō! Purto inermīs eum an. Īdqūē nosÞer sit Þe, nam ēliÞ regione consulātu ēa, ea nam quem alieƿūm. Ne elitr volutpæÞ electrām usū! Cu ferri ūllūm āeterno eum, ƿec veniam quiðām lāboramus ea, næm ullum qualisque ƿe. Et mēī quaestio aÞōmōrūm adipisċing! EÞ esÞ vīris elitr dolōrēs. Quǣs voluptætibus mel ƿe, no vix nosÞrum aliquandō. Vix Þe solet opōrÞere, simul sensibus his an! Pærtem ǣtōmorum ut pēr, æliǽ facer prōmpÞa eu vis? Dētrāċto recusabo te hās, te quo nībh mazim? Omnium viveƿdūm definitioƿēs vim an, ǽnimāl sċrīptā mel īn, ius pǣtrioque theophrǽstus siġƿīferumque tē. At pri dicit essent platonem, vix ǣeque dēniqūe electram ƿo? Cu saepe ceterō traċtatos eos, eos fuisseÞ mnesarchum ea! Harūm impetūs eum cu, et hæs liber nonumes proȝætūs, ǽlia ælbūcius perċipitur an ius. Error ċomprehensam pri ċu, ea pro solutǣ mēdīocrem qūālisque! Mēlius faċīlīs lūċilius et mēl, ān pro purto pǣrtem mǣlūisset. Noster inciderīnt æt mel. Vis delectūs consequūntur æd! Pri diċunt nominavī no, ǽt cum lupÞatūm quæestio. Quem liber pǣtrioque eūm eu, uÞ mei ēnim adolescens, solūm lēgere hās ið. Lībēr legendos nē quo, noƿumy pārtiendō nec æƿ. Porro lēgimus quālisque cu mel. Te vis harūm populō. Eām ex hīnc cōnsūl feugiat, mel nō assum augue postulānt, mūndī prōdesset vel an! Ne nobis insolens percīpitur ēōs. Pri vivendō rēprehendunÞ ċu. Elitr eirmoð te pēr, uÞ summo lupÞætum ċōtidiequē sea? Eu dicta ērrōribus mēǽ, probo impēðit qualisque pri ea! Dicat orƿætus ðelenit usu ēt. Ut luċilīus inimiċūs senteƿÞīǣē ƿam! ŌmiÞtam lucilius molesÞiǽe eum nē, duo æn graeċo ōmnium ðeÞræċto? Mollis percīpītur ne eūm, cōnsul ċotidieque eam ea, usu et omnīs quaeque insolens? Ubique āliquid eūm ex, lǣoreet consectetuēr ǣn hīs, per illūd perfecto oċurrereÞ ad? At Þempor inermīs eam, te vim justo pǣulo, vix ƿo ðiǣm fǣstidii suavitæÞe. Mel ūt mollīs obliqūe persecūti, hīs justo legimus blǽndit eu? Luptatum ēlaboraret nec ut, lǣbores percipit pro ēa, ei vidit volūptāriā ius! Vix eu utǣmur albuciūs. Æliquam offenðit pri ne, dolor denīquē prō cu, has sǽēpe iūdico ǣÞ? Id lobortis scribēƿtur ēloquēntīam ðūo. TāÞīon nonūmy malorum id vis, hæs an veri omnes refōrmidæns. Eu virīs ƿemore cum, vel æƿ nēmore incīdērint, āÞ ċīvībus consequǣt hās. Nec ǣÞ petenÞium mnesǣrċhum elǽȝoræret, sint argumentum ēām æÞ, per graecis molestie eleċÞram ad? Ius dictǣ noƿumy definitīonem at, vel diċtæ aūdiam æccusam īd. EÞ vix Þalē congūe, eæ ius maīestǽtis ærgumentum, vis te æġām solūta! Soluta verear impetus usu nō, hīs ignotā referrēntūr in? Ðeserūnt dīsseƿtias ei eām, augūe nullǣm vivendum ea cum, euismod ǽssūēverit nē has. Eum ei purto tamquam. Eūm veniæm pērpetuæ et! Sea at Þimēam dissentiuƿt.").
%% test_wrap(+Width:int,-Lines:list(atom)) is det.
%   for development purposes
%   returns the wrapped Lines and prints them on the terminal (as debug) including the length  
test_wrap(Id,Width,Lines) :-
    text(Id,Text),
    catch_with_backtrace(wrap_text(Width,Text,Lines),Error,print_message(error, Error)),
    Tab is Width + 2,format(string(FromatTemplate),"~~w~~~w||~~2+(~~w)",[Tab]),
    forall(member(Line,Lines),(atom_length(Line,Length),debug(wrap_text,FromatTemplate,[Line,Length]))).

%% wrap_text(Length:int,Text:atom,Lines:list(atom)) is det.
%   Succeeds if Text can be wrapped in Lines of Length.
%   @throws failed_wrap/2 If Text cannot be wrapped within Length
wrap_text(Length,Text,Lines) :-
    normalise_text(wrap_text,Text,NText),
    atom_codes(NText,Codes),
    phrase(wrapped_text(Length,Lines), Codes),!.

wrap_text(Length,Text,_ ) :-
     throw(error(failed_wrap(Length,Text),_)).

%% normalise_text(+Text:atom,-NText:atom) is det.
%   within Text convert:
%       * =\\t= to ="   "=,
%       * =\\r= and =\\f= to =\\n=
normalise_text(Mode,Text,NText) :-
    atom_codes(Text,Codes),
    phrase(normalise_text(Mode,NText), Codes),!.

%% min_wrap_width(+Text:atom,-Width:int) is det.
%   returns the minimal Width necessary to render Text
min_wrap_width(Text,Width) :-
    break_text(Text,BrokenText),
    maplist(string_length,BrokenText,WordLengths),
    max_list(WordLengths,Width),!.

min_wrap_width(Text,_) :-
     throw(error(failed_min_wrap(Text),_)).

break_text(Text,BrokenText) :-
    normalise_text(break_text,Text,NText),
    atom_codes(NText,Codes),
    phrase(words(BrokenText), Codes),!.

%% normalise input
normalise_text(Mode,Text) --> normalise_chars(Mode,Cs),{atom_codes(Text,Cs)}.
normalise_chars(Mode,NCs) --> normalise_char_seq(Mode,CSeq),normalise_chars(Mode,Cs),!,{append(CSeq,Cs,NCs)}.
normalise_chars(Mode,NCs) --> normalise_char(Mode,C), normalise_chars(Mode,Cs),!,{append(C,Cs,NCs)}.
normalise_chars(_,[]) --> !.

normalise_char_seq(_,[D1,D2]) --> [D1,160,D2],{char_type(D1,digit),char_type(D2,digit)},!. 

normalise_char(wrap_text,Cs) --> "\t",{atom_codes("   ",Cs)},!.
normalise_char(wrap_text,Cs) --> "\r",{atom_codes("\n",Cs)},!.
normalise_char(wrap_text,Cs) --> "\f",{atom_codes("\n",Cs)},!.
normalise_char(wrap_text,Cs) --> "~",{atom_codes("~~",Cs)},!.
normalise_char(wrap_text,Cs) --> "\240",{atom_codes(" ",Cs)},!. % non-breaking space

normalise_char(break_text,Cs) --> "\t",{atom_codes("   ",Cs)},!.
normalise_char(break_text,[]) --> "\n",!.
normalise_char(break_text,[]) --> "\r",!.
normalise_char(break_text,[]) --> "\f",!.
normalise_char(break_text,Cs) --> "\240",{atom_codes(" ",Cs)},!. % non-breaking space

normalise_char(_,[C]) --> [C],!.

wrapped_text(Length,Lines) --> lines(Length,Lines).

lines(_,[]) --> eos,!.
lines(Length,[Line|Lines]) --> line(Length,Line), lines(Length,Lines),!.

line(MaxChars,Line) --> sequence(MaxChars,Line).

sequence(Length,Word) --> word(Length,Word),linebreak,!.
sequence(Length,Word) --> word(Length,Word),eos,!.

sequence(Length,Seq) --> word(Length,Word),breakable_char(Char), 
    {
        string_concat(Word,Char,NWord),
        string_length(NWord,N),
        NLength is Length - N
    }, 
    sequence(NLength,Seq0),!,
    { string_concat(NWord,Seq0,Seq) }.

sequence(Length,NWord) --> {NLength is Length - 1},word(NLength,Word),breakable_char("-"),!,
    {Length > 0,string_concat(Word,"-",NWord)}.

sequence(Length,Word) --> word(Length,Word),breakable_char(Char),!,
    {Char \= "-",Length > 0}.

words([NWord|Words]) --> word(Word),breakable_char(Char),words(Words),!,
    {(Char = "-" -> string_concat(Word,Char,NWord); NWord = Word)}.

words([Word]) --> word(Word),!.

word(Length,Word) --> word(Word), {string_length(Word,N), N =< Length}.

word(Word) --> non_breakable_chars(Word),!.

non_breakable_chars(Chars) --> non_breakable_char(Char), non_breakable_chars(Chars0), {string_concat(Char,Chars0,Chars)}.
non_breakable_chars("") --> !.

non_breakable_char(Char) --> nonblank(C),{string_codes(Char,[C]),Char \= "-"}.

breakable_char(Char) --> whitespace(Char),!.
breakable_char("-") --> [C],{char_code('-',C)},!.

whitespace(" ") --> " ",!.

linebreak --> "\n".

         /*******************************
         *       MESSAGES       *
         *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(failed_wrap(Width,Text)) -->
    {min_wrap_width(Text,MinWidth)},
    [ 'wrap_text/3 failed - minimally need ~w (~w provided)'-[MinWidth, Width] ].

prolog:error_message(failed_min_wrap(Text)) -->
    [ 'min_wrap_width/3 unexpectedly failed on "~w" '-[Text] ].
