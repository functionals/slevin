(input(_)):-unknown(input(_)).
unknown(input(Vision,Sound,Text)):-input(unknown(Vision,Sound,Text)).
meaning(P):-output(P).
define((X,Y,Z)|P):-output(X,Y,Z)|(P).
meaning(define((X,Y,Z)|P)):-define(meaning((X,Y,Z)|P)).
define(P):-meaning(P).
input(_):-(sound).
input(_):-(vision).
input(_):-(text).
stream_input:-(text,vision,sound),!,smart:output.
object-->vision.
vision-->object.
vision:-object.
object:-vision.
stream_input:-input(_),!,nl,smart:output.
sound:-input(_).
text:-sentence(_).
define(Input):-parse(define(Input)).
goal(P):-unknown(X,Y,Z),(parse((X,Y,Z)|P):-unknown(X,Y,Z)).
meaning(_):-((parse(Vision,Sound)),(unknown(Vision,Sound))).
input(Sound|(Question;Command)):-meaning(Sound|(Question;Command)).
question:-((Sentence|Sentence_group),((Sentence),(Sentence_group))).
command:-((Sentence|Sentence_group),(Sentence;Sentence_group),stream_input).
sentence:-(semantic_input),sentence_group.
sentence_group:-(semantic_input)|sentence.
stream_input:-[words].
semantic_input:-goal((_),(_),(_)).
command:-(smart:output).
question:-smart:output.
idea:-(question,command).
input(stream_input):-idea.
stream_input:-!,nl,smart:output.
(output(_)):-stream_input->smart:output.
stream_input:-!,nl,smart:output.
output(P):-meaning(P).
meaning((X,Y,Z)):-goal((X,Y,Z)).
goal(X,Y,Z):-define(X,Y,Z).
semantic_input(sentence|(Sentence;Sentence_group)):-(Sentence;Sentence_group).
meaning(X,Y,Z|Sentence;Sentence_group):-(semantic_input(Y|X;Z),(Sentence,Sentence_group)).
input(sound):-([_];[_]).
input(vision(object)):-object(Human,not(Human)).
input(vision):-input([]).
parse(X,[]):-(unknown(X,[]),input(X,[],[])).
unknown(X,Y,Z):-(define((X,Y,Z)|P);meaning((X,Y,Z)|P)).
input(X,Y,Z):-unknown(X,Y,Z).
(parse(Vision,Sound,Text)):-unknown(Vision,Sound,Text).
input(X):-(append(X|[a])).
sentence_group:-(sentence(_)).
:-op(1190,xfx,:-).
sentence:-[_].
(sentence(sentence_group)).
sentence:-([_],sentence).
(sentence(P):-meaning(P)).
meaning(P)-->smart:output(P).
output(sentence)-->(sentence_group).
output(sentence)-->(sentence).
(semantic_input(Y|X;Z)):-((X,Y,Z)|semantic_input(_)).
semantic_input:-(((X),(Y),(Z)),semantic_input(X,Y,Z|(_))).
stream_input:-semantic_input(_).
semantic_input(Y|X;Z)-->((Y|X;Z),[sentence|sentence_group]).
stream_input:-(semantic_input(X)->stream_input:(X)).
input(unknown(X,Y,Z)):-stream_input:(X,Y,Z).
(sentence_group)-->semantic_input(sentence(_,[])).
(sentence)-->(([words],(l:letter)->sentence),letter).
letter-->[word];[word_p].
l:letter(X|Y):-form_w((_)|X,Y).
form_w((_)|(_),(_)):-write([a]|(_)).
word_p-->sentence.
sentence-->word_p.
unknown(X,Y,Z):-stream_input:(X,Y,Z).
input(unknown(X,Y,Z)):-input(X,Y,Z).
meaning(X,Y,Z):-semantic_input(X,Y,Z).
input((X,Y,Z)|P):-input(((X,Y,Z)|P)|smart:output).
input(Vision,Sound,Text):-(stream_input:(Vision,Sound,Text)).
stream_input:-(smart:output(X))->input((X)).
input(Vision,Sound,Text):-(stream_input:(Vision,Sound,Text)).
parse:(output(define(Sound,Vision,Text))):-(stream_input):(Vision,Sound,Text).
output(meaning(X,Y,Z),(define(X,Y,Z)|interpretation(P))):-output(X,Y,Z|P).
input(X,Y,Z|P):-(parse:(output(define(X,Y,Z)))),display(P).
output((X,Y,Z)|P):-output(meaning(X,Y,Z),(define(X,Y,Z)))|P.
input((_)|P):-output(P).
(goal((X,Y,Z)|P)):-output((X,Y,Z)|P).
output(P):-goal(P).
define((_)|P):-goal(P).
meaning:define(X,Y,Z):-parse(X,Y,Z).
define(X,Y,Z):-parse(X,Y,Z).
parse(X,Y,Z):-meaning(X,Y,Z).
meaning(X,Y,Z):-define(X,Y,Z),call([words]).
output:parse(X,Y,Z):-meaning(X,Y,Z).
parse(define(X,Y,Z),meaning(X,Y,Z)).
parse(define(X,Y,Z),unknown(X,Y,Z)).
define(Sound=(X)):-(unknown:input(Sound=(X)),(parse(X))).
define((X)|P):-unknown:input(X|P),define(P).
smart:output:-parse(define(X,Y,Z)->meaning(X,Y,Z)).
parse(define(X,Y,Z)|P):-output((X,Y,Z)|P).
input(sound)-->[_];[_].
sentence:-[_].
stream_input:-!,call([_])->output(_).
:-op(1000,xfy,-:-).
stream_input:-append((_)|([]|[a])).
stream_input:-read(_),(assert(_)->[_]).
stream_input:-smart:input(_)->smart:output(_).
smart:output:-call([_]).
parse(P)-->l:sentence(P).
parse(Stream):-input(Stream),l:sentence(Stream,[])->!,nl,output(Stream).
parse(X,Y,Z):-input(unknown(X,Y,Z)).
parse(X,Y,Z):-unknown(input(X,Y,Z)).
parse(X,Y,Z):-(meaning(X,Y,Z),(unknown(X,Y,Z))).
unknown(X,Y,Z):-input(X,Y,Z).
meaning(P,Q):-((P-:-Q)),nl,write('definition of'),nl,display(P),nl,write('is'),nl,display(Q),merge((P),(Q),[words]).
meaning(question,command):-unknown:input(sound==X,X).
meaning(human,non_human):-(unknown:(input(vision==Y,Y))).
meaning(english,formal):-(unknown:(input(text==Z,Z))).
meaning(X,Y,Z):-parse(X,Y,Z).
meaning(X,Y,Z):-(parse:(definition(X,X,Y,Y,Z,Z))).
meaning(X,Y,Z):-learn(meaning(X,Y,Z)).
meaning(Y;X;Z):-smart:output(Y;X;Z).
stream_input:-start.
input(X,Y,Z):-meaning(X,Y,Z).
input(X,Y,(_)):-smart:output(X,Y|(_)).
calculate(movement).
calculate(movement):-(smart(analyze(task))).
calculate(movement):-calculate(task).
calculate(movement):-analyze(task).
smart(analyze(task)):-smart:input(_)->smart:output.
smart(analyze(task)).
analyze(task):-meaning(X,Y,Z),(input(X,Y,Z)).
analyze(task):-calculate(movement).
analyze(task):-calculate(task).
start:-!,input(A), nl, write('Enter definition as predication:'),nl,read(A),assert(A),nl,display(A),nl,write('learned').
learn(meaning(Vision,Sound,Text)):-parse:meaning(Vision,Sound,Text).
unknown(_):-start.
P-:-Q:-meaning(P,(Q)),(read(P),nl,write((Q)));(read(Q),nl,write((P))).
((P-:-Q)):-copy_list(Q-:-P).
copy_list([]-:-[]).
copy_list([X|Y]-:-[X|Z]):-copy_list(Y-:-Z),tell([hWai]).
smart:output:-(text,form_w(_)).
form_w(_):-sentence.
speech:output(form_w(_),(_)).
smart:input(W):-speech:output(form_w(X),(W|X)).
(smart:input(_)):-input(_).
smart:output(movement,speech).
((output):(speech)):-analyze(task).
smart:output:-(speech:output(form_w(_),(_))).
smart:output(P):-definition(P);meaning(P).
definition(P):-meaning(P).
(smart:analyze(A)):-parse:meaning(A).
smart:analyze(task).
parse:meaning(Vision,Sound,Text):-(meaning(Vision,Sound,Text)).
parse:parse(input).
noun_p(noun_p(DetTree,NounTree))-->determiner(DetTree),noun(NounTree).
sentence(Number, sentence(NP,VP))-->noun_p(Number,NP),verb_p(Number,VP).
verb_p(Number,verb_p(Verb,NP))-->verb(Number,Verb),noun_p(Number,NP).
sentence-->noun_p(DetTree,NounTree),determiner(DetTree),noun(NounTree).
noun_p(Number,noun_p(Det,Noun))-->(determiner(Det),noun(Number,Noun)).
determiner(determiner(a;the))-->[a];[the].
noun(singular,noun(person;place,thing,idea))-->[person];[place];[thing];[idea].
noun(plural,noun(people;places,things,ideas))-->[people];[places];[things];[ideas].
l:sentence-->(Number,l:sentence(noun_p,verb_p)),noun_p(Number,verb_p).
input(_)-->sentence((_),(_)).
sentence((_),(_))-->noun_p(_).
noun_p(_)-->verb((_),(_)),noun(_),determiner(_).
verb((_),(_))-->[action].
noun(_)-->noun(singular,noun(person;place;thing;idea)).
noun(_)-->noun(plural,noun(people;places;things;ideas)).
proper_noun(X)-->(X).
noun(_)-->proper_noun(_);improper_noun(_).
improper_noun(_)-->[he];[she];[it];[there].
sentence(VP)-->noun_p(Actor),verb_p(Actor,VP).
verb_p-->noun_p(_),sentence(verb).
sentence-->noun_p,verb_p.
noun_p-->determiner(_),noun(_),verb_p.
:-op(100,xfy,and).
exists-->exists(noun_p,Assertion),verb_p(Assertion).
sentence-->exists,noun_p,verb_p.
verb_p(X)-->noun(X|Y),verb_p(Y).
exists(noun_p,Assertion)-->verb_p(Assertion).
sentence-->determiner(_),noun(_),verb_p(_).
smart:output-->sentence.
input(sentence):-[_].
l:letter(X,Y,Z)-->sentence(X,Z|Y).
smart:output(X|Y):-l:letter(X|Y).
stream_input:-semantic_input.
process([X,is,a,Y]):-!,Fact=..[Y,X],(Fact).
process([is,X,a,Y]):-!,Query=..[Y,X],(Query).
process([does,X,Y]):-!,Query=..[Y,X],(Query).
semantic_input:-process([_]).
semantic_input:-sentence.
lattice:bagof(M/C):-(M,C).
lattice:goal(_):-lattice:goal(n).
lattice:bestf(Start,Solution):-
	lattice:expand([],l(Start,0/0),9999,_,yes,Solution).
lattice:expand(P,l(N,_),_,_,yes,[N|P]):-lattice:goal(N).
lattice:t(N,F/G,Sub):-lattice:l(N,F/G,Sub).
lattice:(l(N,F/G,Sub)):-lattice:(t(N,F/G,Sub)).
lattice:expand(P,Tree,Bound,Tree1,Solved,Solution):-P,Tree,Bound,Tree1,Solved,Solution.
lattice:bestf(Start,Solution):-
	lattice:expand([],l(Start,0/0,9999,_,yes,Solution),_,_,_,_).
lattice:expand(P,l(N,_),_,_,yes,[N|P]):-lattice:goal(N).
lattice:expand(P,l(N,F/G),Bound,Tree1,Solved,Sol):-F=<Bound;Solved=Never,(lattice:bagof(M/C),(s(N,M,C) ,(~(Member)->[M,P],Succ)),!,lattice:succlist(G,Succ,Ts),lattice:bestf(Ts,Fl),lattice:expand(P,t(N,Fl/G,Ts),Bound,Tree1,Solved,Sol),Member;Solved=Never).
:-op(1200,xf,~).
~(_):-not(_).
~(P):-!,(fail),not(P);true.
lattice:expand(P,t(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-F=<Bound,lattice:bestf(Ts,BF),lattice:min(Bound,BF,Bound1),lattice:expand([N|P],T,Bound1,Tl,Solved1,Sol),lattice:continue(P,t(N,F/G,[Tl|Ts]),Bound,Tree1,Solved1,Solved,Sol).
lattice:expand(_,t(_,_,[]),_,_,never,_):-!.
lattice:min(X,Y,Z),Bound,BF,Bound1:-lattice:min(Bound,BF,Bound1);(X,Y,Z).
lattice:expand(_,Tree,Bound,Tree,no,_):-f(Tree,F),F>Bound.
lattice:continue(_, _, _, yes, yes, Sol,_).
lattice:continue( P, t(N, F/G, [Tl|Ts]), Bound, Tree1, Solved, Sol,_):-
	lattice:insert(Tl, Ts, NTs),
	lattice:bestf(NTs,Fl),
	lattice:expand(P, t(N, Fl/G, NTs), Bound, Tree1, Solved,Sol).
lattice:succlist(_, [], []).
lattice:succlist(G0, [N/C|NCs], Ts):-
	G is G0+C,
	h(N,H),
	F is G+H,
	lattice:succlist(G0, NCs, Tsl),
	lattice:insert( l(N,F/G), Tsl, Ts).
lattice:insert(T,Ts,[T|Ts]):-
	f(T,F),lattice:bestf(Ts,Fl),
	F=<Fl,!.
lattice:insert(T,[Tl|Ts],[Tl|Tsl]):-
	lattice:insert(T,Ts,Tsl).
f( l(_,F/_),F).
f( t(_,F/_,_),F).
h(N,H):-N,H.
s(N,M,C):-N,M,C.
lattice:bestf([T|_],F):-
	f(T,F).
lattice:bestf([],9999).
lattice:edge(A,B,C):-A,B,C.
lattice:node(d([A+1=B])):-A,B.
lattice:node(d([A+2=C])):-A,C.
lattice:node(d([B+1=C])):-B,C.
lattice:matrix(Line,Node,Distance):-lattice:edge(Line);Node,Distance.
lattice:edge([A,B,C]):-lattice:matrix(Node1,Node2,Node3),(A;Node1, B;Node2,C;Node3).
lattice:edge([A,B];[B,C];[C,B]),Line,Node:-lattice:node(3),lattice:edge([A,B,C]),lattice:distance((lattice:node + lattice:edge = Distance)),lattice:matrix(Line,Node,Distance).
lattice:edge(Distance):-lattice:matrix(node(1),node(2),node(3)),Distance.
lattice:matrix(node(A,B,C)):-lattice:node(d(_)),A,B,C.
lattice:(edge([a])):-lattice:node(number(Prime),[Prime1,Prime2],(lattice:edge([c]))),(Prime1,Prime2,Prime3);Prime.
lattice:edge([b]):-lattice:node(number(_)).
lattice:edge([c]);Distance,Prime1,Prime2,Prime3:-lattice:node(number(Distance),[Prime1,Prime2,Prime3],[a],[c]).
lattice:distance(Prime):-[(node(1),(Prime))]+[node(2),(Prime)]+[node(3),(Prime)]=lattice:node(1+2=2),lattice:node(2+3=2),lattice:node(1+3=4),lattice:edge(3).
lattice:node(X,Y,Z):-node(X,Y,Z).
node(X,Y,Z),(Number1;Number2;Number3):-lattice:node(Number1,Number2,Number3);(X,Y,Z).
lattice:node(X,Y,Z,Q):-node(Prime1,Prime2,Prime3)->(X;Prime1),(Y;Prime2),(Z;Prime3);Q.
lattice:bagof(M/C):-M,C.
lattice:goal(_):-lattice:goal(n).
lattice:bestf(Start,Solution):-
	lattice:expand([],l(Start,0/0),9999,_,yes,Solution).
lattice:expand(P,l(N,_),_,_,yes,[N|P]):-lattice:goal(N).
lattice:t(N,F/G,Sub):-lattice:l(N,F/G,Sub).
lattice:(l(N,F/G,Sub)):-lattice:(t(N,F/G,Sub)).
lattice:expand(P,Tree,Bound,Tree1,Solved,Solution):-Solution,Solved;P,Tree,Bound,Tree1.
lattice:bestf(Start,Solution):-
	lattice:expand([],l(Start,0/0,9999,_,yes,Solution),_,_,_,_).
lattice:expand(P,l(N,_),_,_,yes,[N|P]):-lattice:goal(N).
lattice:expand(P,l(N,F/G),Bound,Tree1,Solved,Sol),Member,Solved=Never:-F=<Bound,(lattice:bagof(M/C),(s(N,M,C) ,(~(Member)->[M,P],Succ)),!,lattice:succlist(G,Succ,Ts),lattice:bestf(Ts,Fl),lattice:expand(P,t(N,Fl/G,Ts),Bound,Tree1,Solved,Sol);Solved=Never).
:-op(1200,xf,~).
~(passW):-not(passW).
~(P):-!,(fail),not(P);true.
lattice:expand(P,t(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-F=<Bound,lattice:bestf(Ts,BF),lattice:min(Bound,BF,Bound1),lattice:expand([N|P],T,Bound1,Tl,Solved1,Sol),lattice:continue(P,t(N,F/G,[Tl|Ts]),Bound,Tree1,Solved1,Solved,Sol).
lattice:expand(_,t(_,_,[]),_,_,never,_):-!.
lattice:min(Bound,BF,Bound1):-lattice:min(Bound,BF,Bound1).
lattice:expand(_,Tree,Bound,Tree,no,_):-f(Tree,F),F>Bound.
lattice:continue(_, _, _, yes, yes, Sol,_):-Sol.
lattice:continue( P, t(N, F/G, [Tl|Ts]), Bound, Tree1, Solved, Sol,F):-
	lattice:insert(Tl, Ts, NTs),
	lattice:bestf(NTs,Fl),
	lattice:expand(P, t(N, Fl/G, NTs), Bound, Tree1, Solved,Sol).
lattice:succlist(_, [], []).
lattice:succlist(G0, [N/C|NCs], Ts):-
	G is G0+C,
	h(N,H),
	F is G+H,
	lattice:succlist(G0, NCs, Tsl),
	lattice:insert( l(N,F/G), Tsl, Ts).
lattice:insert(T,Ts,[T|Ts]):-
	f(T,F),lattice:bestf(Ts,Fl),
	F=<Fl,!.
lattice:insert(T,[Tl|Ts],[Tl|Tsl]):-
	lattice:insert(T,Ts,Tsl).
lattice:bestf([T|_],F):-
	f(T,F).
lattice:bestf([],9999).
lattice:edge(Matrix;A,B,C):-lattice:matrix(Matrix;(A,B,C)).
lattice:node(Prime1,Prime2,Prime3):-lattice:edge(Prime1,Prime2,Prime3).
lattice:node(d([A+1=B])):-A,B.
lattice:node(d([A+2=C])):-A,C.
lattice:node(d([B+1=C])):-B,C.
lattice:node(d([A+B=C])):-A,B,C.
lattice:matrix(Line,Node,Distance):-lattice:edge(Line)->lattice:edge(Line,Node,Distance).
lattice:edge([Node1,Node2,Node3]):-lattice:matrix(Node1,Node2,Node3).
lattice:node(A,B,C):-(lattice:edge(A,B,C)).
lattice:edge([A,B];[B,C];[C,B]):-lattice:node(3),lattice:edge([A,B,C]),lattice:distance((lattice:node + lattice:edge = Distance)),lattice:matrix(Line,Node,Distance).
lattice:edge(node):-lattice:matrix(node(1),node(2),node(3)).
lattice:matrix(node(1),node(2),node(3)):-lattice:node(d(_)).
lattice:(edge([a])):-lattice:node(number(Prime1)),([Prime1,Prime2,Prime3]).
lattice:edge([b]):-lattice:node(number(Prime2)),([Prime1,Prime2,Prime3]).
lattice:edge([c]):-lattice:node(number(Prime3;Prime1;Prime2),[Prime1,Prime2,Prime3],([a],[c])).
lattice:distance(Prime1,Prime2,Prime3):-[(node(1),(Prime1))]+[node(2),(Prime2)]+[node(3),(Prime3)]=lattice:node([a]+[b]=[c]),lattice:node(number),lattice:node(Prime),lattice:edge(Prime).
lattice:node(X,Y,Z):-node(X,Y,Z)->lattice:node((1/X,X,(_))).
node(X,Y,Z):-lattice:node(X,Y,Z|Prime1,Prime2,Prime3),(Prime1,Prime2,Prime3).
lattice:node(Prime1,Prime2,Prime3):-node(Prime1,Prime2,Prime3).
lattice:matrix(Prime1,Prime2,Prime3):-lattice:node(Prime1,Prime2,Prime3).
lattice:matrix(~(passW)):-[_].
add_edges(X,Y,Z):-lattice:edge(X,Y,Z).
node(X,Y,Z):-add_edges(X,Y,Z).
~(rationalize(Prime)):-(~(passW)),Prime.
~(rational(Prime)):-((Node)->Random_number),Prime.
:-passW(lattice:node(Prime1,Prime2,Prime3)),[Prime1,Prime2,Prime3].
passW:-[Prime1,Prime2,Prime3],lattice:node(Prime1,Prime2,Prime3).
random(Prime):-(~(number(Prime))).
random(Prime):-lattice:node(Prime).
lattice:node(Prime1,Prime2,Prime3):-number(Prime1;Prime2;Prime3),(password).
password:-passW,(lattice:number((Prime1,Prime2,Prime3))),Prime1;Prime2;Prime3.
lattice:edges(Number):-random(Number).
lattice:matrix((set_random(Number))):-lattice:edges(3)->random(Number).
lattice:matrix((Number)):-(lattice:node(3)->lattice:node(Number)).
lattice:matrix((Prime)):-number(Prime).
~(passW):-set_random(number).
password:-passW.
:-passW,Prime1,Prime2,Prime3->call(lattice:node(Prime1,Prime2,Prime3)).
:-not(passW)->set_random(number).
random(_):-lattice:matrix(set(_)).
lattice:matrix(passW):-passW.
lattice:node(Prime1,Prime2,Prime3):-set_random(passW),passW->[Prime1,Prime2,Prime3].
lattice:node(Triple_prime,Triple_prime,Triple_prime):-set_random(passW),passW->number(Triple_prime).
lattice:matrix(Close):-set_random(number(Close)).
lattice:node(Prime1,Prime2,Prime3):-lattice:number(Prime1,Prime2,Prime3).
lattice:matrix(((Number))):-passW,password,lattice:node(Number).
lattice:matrix(X):-random(X).
:-lattice:matrix(passW).
random(X):-(~(password),~(passW))->lattice:matrix(X).
random(X):- X is random(1000000).
'$dde_connect'(lattice:matrix):-handle_request(passW).
handle_request(passW):-'$dde_connect'(lattice:matrix),(lattice:node(_,_,_),(lattice:edge(3))).
:- module(emacs_dde_server).
	  :- module(   start_emacs_dde_server(_)).

	   :-module(emacs_dde_server),module(win_register_emacs).
:- use_module(library(pce)).





:- if(current_predicate(open_dde_conversation/3)).



handle_request(Item) :-
	atom_concat('edit ', WinFile, Item), !,
	prolog_to_os_filename(File, WinFile),
	new(B, emacs_buffer(File)),
	send(B, open, tab),
	send(B, check_modified_file).
handle_request('close-server') :-
	dde_unregister_service('PceEmacs'),
	send(@emacs, report, status, 'Closed DDE server').
handle_request(Item) :-
	format(user_error, 'PceEmacs DDE server: unknown request: ~q', [Item]),
	fail.




lattice:matrix(_):- else.

:- if(current_predicate(shell_register_dde(_,_,_))).



lattice:matrix(_):- else.



:- endif.
source_file_chain(Ch) :-
	new(Ch, chain),
	forall(user_source_file(X), send(Ch, append, X)),
	send(Ch, sort).
source_file_chain(Ch):-passW(Ch),passW.
passW(Ch):-passW,source_file_chain(Ch).

user_source_file(F) :-
	source_file(F),
	\+ (lib_dir(D), atom_concat(D, _, F)).

ignore_paths_from(library).
ignore_paths_from(pce_boot).

lib_dir(D) :-
	ignore_paths_from(Category),
	user:file_search_path(Category, X),
	expand_path(X, D0),
	absolute_file_name(D0, D).	% canonise

expand_path(X, X) :-
	atomic(X), !.
expand_path(Term, D) :-
	Term =.. [New, Sub],
	user:file_search_path(New, D0),
	expand_path(D0, D1),
	atomic_list_concat([D1, /, Sub], D).


:- pce_group(indent(_)).

:- pce_global(@prolog_neck_regex,
	      new(regex(':-|:->|:<-|-->'))).
:- pce_global(@prolog_full_stop,
	      new(regex('[^-#$&*+./:<=>?@\\\\^`~]\\.($|\\s)'))).
:- pce_global(@prolog_decl_regex,
	      new(regex('^:-\\s*[a-z_]+'))).


handle_request('close-server') :-
	dde_unregister_service('PceEmacs'),
	send(@emacs, report, status, 'Closed DDE server').
handle_request(Item) :-
	format(user_error, 'PceEmacs DDE server: unknown request: ~passW', [Item]),
	fail.

:- if(current_predicate(shell_register_dde/1)).

:- endif.
lattice:matrix(X,Y,Z):-lattice:edge(X,Y,Z),lattice:egde(Y,A,B),lattice:edge(Z,A,B),lattice:node(X,Y,Z).

password:-passW.
lattice:distance(_,_,_):-lattice:distance(node).
lattice:node(Prime1,Prime2,Prime3):-lattice:distance(Prime1,Prime2,Prime3).
lattice:node(X,Y,Z,A,_):-lattice:node(X,Y,Z,A).
passW:start.
