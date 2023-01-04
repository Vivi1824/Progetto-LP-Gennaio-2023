%Progetto Linguaggi di Programmazione
%15 Gennaio 2023
%Viviana Giuliani 875068
%Daniel Marco Gatti 869310

jsonparse(JString, Object) :-
    string_codes(JString, Chars),
    remove_ws(Chars, Chars1),
    json_object(Chars1, Chars2, [], Object),
   remove_ws(Chars2, Chars3),
    list_is_empty(Chars3),
    !.

jsonparse(JString, Object) :-
    string_codes(JString, Chars),
   remove_ws(Chars, Chars1),
    jsonarray(Chars1, Chars2, [], Object),
   remove_ws(Chars2, Chars3),
    list_is_empty(Chars3),
    !.

json_object(CharsIn, CharsOut, ObjectIn, jsonobj(ObjectOut)) :-
    first_char("{", CharsIn, Chars1),
   remove_ws(Chars1, Chars2),
    first_char("}", Chars2, CharsOut),
    !,
    ObjectIn = ObjectOut.
json_object(CharsIn, CharsOut, ObjectIn, jsonobj(ObjectOut)) :-
    first_char("{", CharsIn, Chars1),
    !,
   remove_ws(Chars1, Chars2),
    members(Chars2, Chars3, ObjectIn, ObjectOut),
   remove_ws(Chars3, Chars4),
    first_char("}", Chars4, CharsOut).

jsonarray(CharsIn, CharsOut, ObjectIn, jsonarray(ObjectOut)) :-
    first_char("[", CharsIn, Chars1),
   remove_ws(Chars1, Chars2),
    first_char("]", Chars2, CharsOut),
    !,
    ObjectIn = ObjectOut.
jsonarray(CharsIn, CharsOut, ObjectIn, jsonarray(ObjectOut)) :-
    first_char("[", CharsIn, Chars1),
    !,
   remove_ws(Chars1, Chars2),
    elements(Chars2, Chars3, ObjectIn, ObjectOut),
   remove_ws(Chars3, Chars4),
    first_char("]", Chars4, CharsOut).

members(CharsIn, CharsOut, ObjectIn, ObjectOut1) :-
    pairing(CharsIn, Chars2, ObjectIn, ObjectOut),
   remove_ws(Chars2, Chars3),
    first_char(",", Chars3, Chars4),
   remove_ws(Chars4, Chars5),
    members(Chars5, CharsOut, ObjectOut, ObjectOut1),
    !.
members(CharsIn, CharsOut, ObjectIn, ObjectOut) :-
    pairing(CharsIn, CharsOut, ObjectIn, ObjectOut),
    !.

elements(CharsIn, CharsOut, ObjectIn, ObjectOut2) :-
    value(CharsIn, Chars2, ObjectOut),
   remove_ws(Chars2, Chars3),
    first_char(",", Chars3, Chars4),
   remove_ws(Chars4, Chars5),
    !,
    append(ObjectIn, [ObjectOut], ObjectOut1),
    elements(Chars5, CharsOut, ObjectOut1, ObjectOut2).
elements(CharsIn, CharsOut, ObjectIn, ObjectOut1) :-
    value(CharsIn, CharsOut, ObjectOut),
    append(ObjectIn, [ObjectOut], ObjectOut1),
    !.


pairing(CharsIn, CharsOut, ObjectIn, ObjectOut) :-
    json_string(CharsIn, Chars2, Key),
   remove_ws(Chars2, Chars3),
    first_char(":", Chars3, Chars4),
   remove_ws(Chars4, Chars5),
    value(Chars5, CharsOut, Value),
    append(ObjectIn, [(Key,Value)], ObjectOut).


json_string(CharsIn, CharsOut, Key) :-
    first_char("\'", CharsIn, Chars2),
    !,
    creation_ss(Chars2, Chars3, Result),
    first_char("\'", Chars3, CharsOut),
    string_codes(Key, Result).
json_string(CharsIn, CharsOut, Key) :-
    first_char("\"", CharsIn, Chars2),
    !,
    creation_sq(Chars2, Chars3, Result),
    first_char('\"', Chars3, CharsOut),
    string_codes(Key, Result).


value(CharsIn, CharsOut, Object) :-
    json_string(CharsIn, CharsOut, Object),
    !.
value(CharsIn, CharsOut, Object) :-
    jmix(CharsIn, CharsOut, Object),
    !.
value(CharsIn, CharsOut, Object) :-
    number(CharsIn, CharsOut, Object),
    !.

number(CharsIn, CharsOut, Object) :-
    first_char("-", CharsIn, Chars2),
    char_code('-', Minus),
    number_creation(Chars2, Chars3, Value1),
    list_is_not_empty(Value1),
    append([Minus], Value1, Value1Minus),
    first_char(".", Chars3, Chars4),
    !,
    char_code('.', Dot),
    append(Value1Minus, [Dot], Value2),
    number_creation(Chars4, CharsOut, Value3),
    list_is_not_empty(Value3),
    append(Value2, Value3, Value),
    number_codes(Object, Value).


number(CharsIn, CharsOut, Object) :-
    first_char("-", CharsIn, Chars2),
    char_code('-', Minus),
    number_creation(Chars2, CharsOut, Value2),
    list_is_not_empty(Value2),
    !,
    append([Minus], Value2, Value),
    number_codes(Object, Value).

number(CharsIn, CharsOut, Object) :-
    number_creation(CharsIn, Chars1, Value1),
    first_char(".", Chars1, Chars2),
    char_code('.', Dot),
    append(Value1, [Dot], Value1Dot),
    !,
    number_creation(Chars2, CharsOut, Value2),
    append(Value1Dot, Value2, Value),
    list_is_not_empty(Value),
    number_codes(Object, Value).
number(CharsIn, CharsOut, Object) :-
    number_creation(CharsIn, CharsOut, Value),
    list_is_not_empty(Value),
    number_codes(Object, Value).


list_is_not_empty(List) :- List \= [], !.

list_is_empty(List) :- List = [], !.


jmix(CharsIn, CharsOut, Object) :-
    json_object(CharsIn, CharsOut, [], Object),
    !.
jmix(CharsIn, CharsOut, Object) :-
    jsonarray(CharsIn, CharsOut, [], Object),
    !.

first_char(Char, [X | Xs], Chars2) :-
    string_codes(Char, [Y | _]),
    Y = X,
    Chars2 = Xs.

number_creation([X | Xs], [X | Xs], []) :-
    X < 48,
    !.
number_creation([X | Xs], [X | Xs], []) :-
    X > 57,
    !.
number_creation([X | Xs], Zs, [X | Ys]) :-
    number_creation(Xs, Zs, Ys).

creation_ss([X | _], _, _) :-
    string_codes("\"", [Char | _]),
    X = Char,
    !,
    fail.
creation_ss([X | Xs], [X | Xs], []) :-
    string_codes("\'", [Char | _]),
    X = Char,
    !.
creation_ss([X | Xs], Zs, [X | Ys]) :-
    creation_ss(Xs, Zs, Ys).

creation_sq([X | _], _, _) :-
    string_codes("\'", [Char | _]),
    X = Char,
    !,
    fail.
creation_sq([X | Xs], [X | Xs], []) :-
    string_codes("\"", [Char | _]),
    X = Char,
    !.
creation_sq([X | Xs], Zs, [X | Ys]) :-
    creation_sq(Xs, Zs, Ys).

remove_ws([],[]) :- !.
remove_ws([X | Xs], Ys) :-
    is_whitespace_or_newline(X),
    !,
   remove_ws(Xs, Ys).
remove_ws([X | Xs], Ys) :-
    Ys = [X | Xs],
    !.

is_whitespace_or_newline(X) :-
    is_whitespace_custom(X),
    !.
is_whitespace_or_newline(X) :-
    is_newline_custom(X),
    !.
is_whitespace_or_newline(X) :-
    is_tab_custom(X),
    !.

is_whitespace_custom(X) :-
    char_code(' ', Y),
    X = Y,
    !.
is_newline_custom(X) :-
    char_code('\n', Y),
    X = Y,
    !.
is_tab_custom(X) :-
    char_code('\t', Y),
    X = Y,
    !.

jsonaccess(_, [], _) :- !, fail.

jsonaccess(jsonobj(), _, _) :- !, fail.
jsonaccess(jsonarray(), _, _) :- !, fail.

jsonaccess(JSON_obj, [X], Result) :-
    jsonaccess_elements(JSON_obj, X, Result),
    !.
jsonaccess(JSON_obj, [X|Xs], Result) :-
    jsonaccess_elements(JSON_obj, X, Temp),
    !,
    jsonaccess(Temp, Xs, Result).
jsonaccess(JSON_obj, X, Result) :-
    jsonaccess_elements(JSON_obj, X, Result),
    !.

jsonaccess_elements(JSON_obj, Fields, Result) :-
    jsonobj([Y|Ys]) = JSON_obj,
    !,
    jsonaccess_member([Y|Ys], Fields, Result).


jsonaccess_elements(JSON_obj, Index , Result) :-
    jsonarray([X|Xs]) = JSON_obj,
    !,
    jsonaccess_member_position([X | Xs], Index, Result).


jsonaccess_member([], _, _) :- fail.
jsonaccess_member([(X,Y)| _], Z, Result) :-
    string(Z),
    X = Z,
    !,
    Result = Y.
jsonaccess_member([_| Xs], Z, Result) :-
    string(Z),
    jsonaccess_member(Xs, Z, Result).


jsonaccess_member_position([],[_], _) :- fail.
jsonaccess_member_position([X | _], Y, Result) :-
    number(Y),
    Y = 0,
    !,
    Result = X.
jsonaccess_member_position([_ | Xs], Y, Result) :-
    number(Y),
    Z is Y-1,
    jsonaccess_member_position(Xs, Z, Result).


jsonread(Filename, JSON) :-
    open(Filename, read, In),
    read_stream_to_codes(In, X),
    close(In),
    atom_codes(JString, X),
    jsonparse(JString, JSON).

jsondump(JSON, Filename) :-
    open(Filename, write, Out),
    json_print(JSON, JString),
    write(Out, JString),
    close(Out).

json_print(JSON, JString) :-
    JSON = jsonobj([]),
    !,
    JString = "{}".
json_print(JSON, JString) :-
    json_obj([Y | Ys]) = JSON,
    !,
    concat("", "{", JString1),
    json_print_object([Y | Ys], "", JString2),
    concat(JString1, JString2, JString3),
    concat(JString3, "}", JString).

json_print(JSON, JString) :-
    JSON = jsonarray([]),
    !,
    JString = "[]".
json_print(JSON, JString) :-
    jsonarray([Y | Ys]) = JSON,
    !,
    concat("", "[", JString1),
    json_print_array([Y | Ys], "", JString2),
    concat(JString1, JString2, JString3),
    concat(JString3, "]", JString).

json_print_object([], JString, Result) :-
    !,
    string_concat(Temp, ",", JString),
    Result = Temp.
json_print_object([(X,Y)| Xs], JString, Result) :-
    json_print_element(X, JString1),
    string_concat(JString, JString1, JString2),
    string_concat(JString2, ":", JString3),
    json_print_element(Y, JString4),
    string_concat(JString3, JString4, JString5),
    string_concat(JString5, ",", JString6),
    json_print_object(Xs, JString6, Result).

json_print_array([], JString, Result) :-
    !,
    string_concat(Temp, ",", JString),
    Result = Temp.
json_print_array([X| Xs], JString, Result) :-
    json_print_element(X, JString1),
    string_concat(JString, JString1, JString2),
    string_concat(JString2, ",", JString3),
    json_print_array(Xs, JString3, Result).

json_print_element(X, Result) :-
    number(X),
    !,
    Result = X.
json_print_element(X, Result) :-
    json_print(X, Result),
    !.
json_print_element(X, Result) :-
    string(X),
    !,
    string_concat("", "\"", JString1),
    string_concat(JString1, X, JString2),
    string_concat(JString2, "\"", JString3),
    Result = JString3.




