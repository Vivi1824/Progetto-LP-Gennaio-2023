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

%json object
%Caso in cui dentro � vuoto
json_obj(CharsIn, CharsOut, ObjectIn, jsonobj(ObjectOut)) :-
    f_char("{", CharsIn, Chars1),
    remove_ws(Chars1, Chars2),
    f_char("}", Chars2, CharsOut), !,
    ObjectIn = ObjectOut.
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
%json value
json_value(CharsIn, CharsOut, Object) :-
    json_string(CharsIn, CharsOut, Object), !.
json_value(CharsIn, CharsOut, Object) :-
    object_nested(CharsIn, CharsOut, Object), !.



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
members(CharsIn, CharsOut, ObjectIn, ObjectOut) :-
    .







