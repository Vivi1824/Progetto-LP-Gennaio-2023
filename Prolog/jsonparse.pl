json_parse(String, Object):-
    string_codes(String, Chars),
    remove_ws(Chars, Chars1),
    json_obj(Chars1, Chars2, [], Object),
    remove_ws(Chars2, Chars3),
    list_is_empty(Chars3), !.

json_parse(String, Object):-
    string_codes(String, Chars),
    remove_ws(Chars,Chars1),
    json_array(Chars1, Chars2, [], Object),
    remove_ws(Chars2, Chars3),
    list_is_empty(Chars3), !.

%controllo e rimozione spazi bianchi, le linee e i tab
white_space(X) :- char_code(' ', Y), X = Y, !.
new_line(X) :- char_code('\n', Y), X = Y, !.
new_tab(X) :- char_code('\t', Y), X = Y, !.
control_ws(X) :- white_space(X), !.
control_ws(X) :- new_line(X), !.
control_ws(X) :- new_tab(X), !.
remove_ws([], []):- !.
remove_ws([X | Xs], Ys) :- control_ws(X), !, remove_ws(Xs, Ys).
remove_ws([X | Xs], Ys) :- Ys = [X | Xs], !.

%controllo primo carattere
f_char(Char, [X | Xs], Chars2) :- string_codes(Char, [Y | _]),
    Y = X,
    Chars2 = Xs.

creation_number([X|Xs], [X|Xs],[]):-
    X<48,
    !.
creation_number([X|Xs],[X|Xs],[]):-
    X>57,!.
creation_number([X|Xs],Zs,[X|Ys]):-
    creation_number(Xs,Zs,Ys).

%json object
%Caso in cui dentro è vuoto
json_obj(CharsIn, CharsOut, ObjectIn, jsonobj(ObjectOut)) :-
    f_char("{", CharsIn, Chars1),
    remove_ws(Chars1, Chars2),
    f_char("}", Chars2, CharsOut), !,
    ObjectIn = ObjectOut.
json_obj(CharsIn, CharsOut, ObjectIn, jsonobj(ObjectOut)):-
    f_char("{", CharsIn, Chars1),
    !,
    remove_ws(Chars1, Chars2),
    members(Chars2,Chars3, ObjectIn, ObjectOut),
    remove_ws(Chars3, Chars4),
    f_char("}", Chars4, CharsOut).


json_string(CharsIn, CharsOut, Key) :-
    f_char("\'", CharsIn, Chars2), !,
    creation_ss(Chars2, Chars3, Result),
    f_char("\'", Chars3, CharsOut),
    string_codes(Key, Result).
json_string(CharsIn, CharsOut, Key) :-
    f_char("\"", CharsIn, Chars2), !,
    creation_ss(Chars2, Chars3, Result),
    f_char("\"", Chars3, CharsOut),
    string_codes(Key, Result).

%se numero positivo
number(CharsIn, CharsOut, Object):-
    f_char("+", CharsIn, Chars2),
    char_code('+', Plus),
    creation_number(Chars2, Chars3, Value1),
    list_not_empty(Value1),
    append([Plus], Value1, Value1Minus),
    f_char(".", Chars3, Chars4),!,
    char_code('.',Dot),
    append(Value1Minus,[Dot], Value2),
    creation_number(Chars4,CharsOut,Value3),
    list_not_empty(Value3),
    append(Value2, Value3, Value),
    number_codes(Object,Value).
number(CharsIn, CharsOut, Object):-
    f_char("+",CharsIn,Chars2),
    char_code('+',Plus),
    creation_number(Chars2, CharsOut, Value2),
    list_not_empty(Value2),!,
    append([Plus], Value2, Value),
    number_codes(Object,Value).

%se numero negativo
number(CharsIn,CharsOut,Object):-
    f_char("-",CharsIn, Chars2),
    char_code('-',Minus),
    creation_number(Chars2, Chars3, Value1),
    list_not_empty(Value1),
    append([Minus], Value1, Value1Minus),
    f_char(".", Chars3, Chars4),!,
    char_code(".",Dot),
    append(Value1Minus,[Dot],Value2),
    creation_number(Chars4, CharsOut, Value3),
    list_not_empty(Value3),
    append(Value2, Value3, Value),
    number_codes(Object,Value).
number(CharsIn, CharsOut, Object):-
    f_char("-",CharsIn, Chars2),
    char_code('-', Minus),
    creation_number(Chars2, CharsOut, Value2),
    list_not_empty(Value2),!,
    append([Minus], Value2, Value),
    number_codes(Object, Value).

%se il numero è float
number(CharsIn, CharsOut, Object):-
    creation_number(CharsIn, Chars1, Value1),
    f_char(".",Chars1, Chars2),
    char_code('.',Dot),
    append(Value1, [Dot], Value1Dot),!,
    creation_number(Chars2, CharsOut, Value2),
    append(Value1Dot, Value2, Value),
    list_not_empty(Value),
    number_codes(Object, Value).
number(CharsIn, CharsOut, Object):-
    creation_number(CharsIn, CharsOut, Value),
    list_not_empty(Value),
    number_codes(Object,Value).

%json value
json_value(CharsIn, CharsOut, Object) :-
    json_string(CharsIn, CharsOut, Object), !.
json_value(CharsIn, CharsOut, Object) :-
    object_nested(CharsIn, CharsOut, Object), !.
json_value(CharsIn, CharsOut, Object):-
    number(CharsIn, CharsOut, Object),!.

%creazione stringhe inizio
creation_ss([X | _], _, _) :- string_codes("\"", [Char | _]),
    X = Char, !, fail.
creation_ss([X | Xs], [X | Xs], []) :- string_codes("\'", [Char | _]),
    X = Char, !.
screation_ss([X | Xs], Zs, [X | Ys]) :- creation_ss(Xs, Zs, Ys).
%creazione stringhe fine
creation_ds([X | _], _, _) :- string_codes("\'", [Char | _]),
    X = Char, !, fail.
creation_ds([X | Xs], [X | Xs], []) :- string_codes("\"", [Char | _]),
    X = Char, !.
creation_ds([X | Xs], Zs, [X | Ys]) :- creation_ds(Xs, Zs, Ys).

%controllo liste vuote
list_not_empty(List):- List \= [], !.
list_is_empty(List):- List = [], !.

%oggetti innestati tra di loro
object_nested(CharsIn, CharsOut, Object)
        :-json_obj(CharsIn, CharsOut, [], Object),!.
object_nested(CharsIn, CharsOut, Object)
        :-json_array(CharsIn, CharsOut, [], Object), !.

pair(CharsIn, CharsOut, ObjectIn, ObjectOut) :-
    json_string(CharsIn, Chars2, Key),
    remove_ws(Chars2, Chars3),
    f_char(":",Chars3, Chars4),
    remove_ws(Chars4, Chars5),
    json_value(Chars5, CharsOut, Value),
    append(ObjectIn, [(Key, Value)], ObjectOut).
is_number([X | Xs], [X | Xs], []) :-
    X < 48, !.
is_number([X | Xs], [X | Xs], []) :-
    X > 57, !.
is_number([X | Xs], Zs, [X | Ys]) :-
    is_number(Xs, Zs, Ys).
members(CharsIn, CharsOut, ObjectIn, ObjectOut1) :-
   pair(CharsIn, Chars2, ObjectIn, ObjectOut),
   remove_ws(Chars2, Chars3),
   f_char(",", Chars3, Chars4),
   remove_ws(Chars4, Chars5),
   members(Chars5, CharsOut, ObjectOut, ObjectOut1),!.
members(CharsIn, CharsOut, ObjectIn, ObjectOut):-
    pair(CharsIn, CharsOut, ObjectIn,ObjectOut),!.

elements(CharsIn, CharsOut, ObjectIn, ObjectOut2):-
    json_value(CharsIn, Chars2, ObjectOut),
    remove_ws(Chars2, Chars3),
    f_char(",", Chars3, Chars4),
    remove_ws(Chars4, Chars5),!,
    append(ObjectIn,[ObjectOut], ObjectOut1),
    elements(Chars5, CharsOut, ObjectOut1, ObjectOut2).
elements(CharsIn,CharsOut,ObjectIn,ObjectOut1):-
    json_value(CharsIn, CharsOut, ObjectOut),
    append(ObjectIn, [ObjectOut], ObjectOut1), !.


%Controllo Array
json_array(CharsIn, CharsOut, ObjectIn, jsonarray(ObjectOut)):-
    f_char("[", CharsIn, Chars1),
    remove_ws(Chars1, Chars2),
    f_char("]", Chars2, CharsOut),!,
    ObjectIn = ObjectOut.

json_array(CharsIn, CharsOut, ObjectIn, jsonarray(ObjectOut)):-
    f_char("[", CharsIn, Chars1),
    !,
    remove_ws(Chars1, Chars2),
    elements(Chars2, Chars3, ObjectIn, ObjectOut),
    remove_ws(Chars3, Chars4),
    f_char("]", Chars4, CharsOut).

%Fallisce se l'elemento è vuoto
get(_,[],_):-!, fail.
%Ritorna un identity se il campo è vuoto
get(X,void,X):-!.
%Non trova nulla in un oggetto/array
get(json_obj(),_,_):-!, fail.
get(json_array(),_,_):-!, fail.
%Altrimenti
get(Obj, [X], Result):- get_elements(Obj, X, Result), !.
get(Obj, [X|Xs], Result):- get_elements(Obj, X, Temp), !,
    get(Temp, Xs, Result).
get(Obj, X, Result):- get_elements(Obj, X, Result), !.


%Prende gli elementi
get_elements(Obj, Fields, Result):- json_obj([Y|Ys])=Obj,!,
    get_member([Y|Ys], Fields, Result).
get_elements(Obj, Index, Result):- json_array([X|Xs])=Obj,!,
    get_member_pos([X|Xs], Index, Result).

get_member([],_,_):- fail.
get_member([(X,Y)|_], Z, Result):- string(Z), X=Z, !,
    Result = Y.
get_member([_|Xs],Z,Result):- string(Z),
    get_member(Xs, Z, Result).


%Cerca un elemento data la posizione
get_member_pos([],[_],_):-fail.
get_member_pos([X|_], Y, Result):- number(Y), Y=0, !, Result=X.
get_member_pos([_|Xs], Y, Result):- number(Y), Z is Y-1,
    get_member_pos(Xs, Z, Result).



%Lettura e Scrittura su file
jsonread(Filename, JSON):- open(Filename, read, In),
    read_stream_to_codes(In, X), close(In),
    atom_codes(String, X),json_parse(String, JSON).

jsondump(JSON, Filename):- open(Filename, write, Out),
    json_print(JSON, String), write(Out, String),
    close(Out).

json_print(JSON, String):- JSON=json_obj([]), !,
    String = "{}".
json_print(JSON, String):- json_obj([Y|Ys]) = JSON, !,
    concat("", "{", String1), print_object([Y|Ys], "", String2),
    concat(String1, String2, String3),
    concat(String3, "}", String).
json_print(JSON, String):- JSON = json_array([]), !,
    String = "[]".
json_print(JSON, String):- json_array([Y|Ys]) = JSON, !,
    concat("", "[", String1), print_array([Y|Ys], "", String2),
    concat(String1, String2, String3),
    concat(String3, "]", String).

print_object([], String, Result):-!,
    string_concat(Temp,",", String), Result = Temp.
print_object([(X,Y)|Xs],String, Result):-
    print_element(X,String1),
    string_concat(String, String1, String2),
    string_concat(String2, ":", String3),
    print_element(Y, String4),
    string_concat(String3, String4, String5),
    string_concat(String5, ",", String6),
    print_object(Xs, String6, Result).

print_array([], String, Result):-!,
    string_concat(Temp, ",", String), Result = Temp.
print_array([X|Xs], String, Result):-
    print_element(X, String1),
    string_concat(String, String1, String2),
    string_concat(String2, ",", String3),
    print_array(Xs, String3, Result).

print_element(X, Result):- number(X), !, Result = X.
print_element(X, Result):- json_print(X, Result), !.
print_element(X, Result):- string(X), !,
    string_concat("","\"", String1),
    string_concat(String1, X, String2),
    string_concat(String2,"\"", String3),
    Result = String3.

