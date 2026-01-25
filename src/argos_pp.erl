-module('argos_pp').
-export([print/2]).


print(Term, Opt) 
    when is_map(Term),is_map(Opt)
    ->
    IndLen = case maps:get(indent, Opt, 3) of
                X when is_list(X) -> string:length(X);
                X when is_integer(X) -> X ;
                _ -> 3
             end,
    Fun = fun(Value, Encode, State) -> formatter(Value, Encode, State) end,
    FOpt = #{indent => IndLen, style => maps:get(style, Opt, '') },
    io:put_chars(json:format(Term, Fun, FOpt)).


formatter(Other, Encode, State) -> format_value(Other, Encode, State).

format_value(Atom, UserEnc, State) when is_atom(Atom) ->
    json:encode_atom(Atom, fun(Value, Enc) ->  UserEnc(Value, Enc, State) end);
format_value(Bin, _Enc, _State) when is_binary(Bin) ->
    json:encode_binary(Bin);
format_value(Int, _Enc, _State) when is_integer(Int) ->
    json:encode_integer(Int);
format_value(Float, _Enc, _State) when is_float(Float) ->
    json:encode_float(Float);
format_value(List, UserEnc, State) when is_list(List) ->
    case io_lib:printable_list(List) of
        true  -> json:encode_binary(list_to_binary(List)) ;
        false -> format_list(List, UserEnc, State)
    end;
format_value(Map, UserEnc, State) when is_map(Map) ->
    %% Ensure order of maps are the same in each export
    OrderedKV = maps:to_list(maps:iterator(Map, ordered)),
    format_key_value_list(OrderedKV, UserEnc, State);
format_value(Other, _Enc, _State) ->
    error({unsupported_type, Other}).

format_list([Head|Rest], UserEnc, #{level := Level, col := Col0, max := Max, style := Style} = State0) ->
    State1 = State0#{level := Level+1},
    {Len, IndentElement} = indent(State1),
    State2 = State0#{level := Level+2},
    {_, IndentElement2} = indent(State2),
    if is_list(Head);   %% Indent list in lists
       is_map(Head);    %% Indent maps
       is_binary(Head); %% Indent Strings
       Col0 > Max ->    %% Throw in the towel
            State = State1#{col := Len},
            First = case Style of
                        'gnu' -> UserEnc(Head, UserEnc, maps:update(level, maps:get(level, State0, 1) , State0));
                        'whitesmiths' -> UserEnc(Head, UserEnc, maps:update(level, maps:get(level, State1, 1) , State1));
                        _ -> UserEnc(Head, UserEnc, State)
                    end,
            {_, IndLast} = indent(State0),
            case Style of
            'allman' ->  
                [IndLast, $[, IndentElement, First,
                    format_tail(Rest, UserEnc, State, IndentElement, IndentElement),
                    IndLast, $] ];
            'hortsmann' ->  
                [IndLast, $[, IndentElement, First,
                    format_tail(Rest, UserEnc, State, IndentElement, IndentElement),
                    IndLast, $] ];
            'whitesmiths' ->  
                [IndentElement, $[, IndentElement2, First,
                    format_tail(Rest, UserEnc, State2, IndentElement2, IndentElement2),
                    IndentElement, $] ];
            _ -> 
                [$[, IndentElement, First,
                    format_tail(Rest, UserEnc, State, IndentElement, IndentElement),
                    IndLast, $] ]
            end;
       true ->
            First = UserEnc(Head, UserEnc, State1),
            Col = Col0 + 1 + erlang:iolist_size(First),
            case Style of 
            _ -> 
                [$[, First,
                 format_tail(Rest, UserEnc, State1#{col := Col}, [], IndentElement),
                 $] ]
            end
    end;
format_list([], _, _) ->
    <<"[]">>.

format_tail([Head|Tail], Enc, #{max := Max, col := Col0, style := _Style} = State, [], IndentRow)
  when Col0 < Max -> 
    EncHead = Enc(Head, Enc, State),
    String = [$,|EncHead],
    Col = Col0 + 1 + erlang:iolist_size(EncHead),
    [String|format_tail(Tail, Enc, State#{col := Col}, [], IndentRow)];
format_tail([Head|Tail], Enc, State, [], IndentRow) -> 
    EncHead = Enc(Head, Enc, State),
    String = [[$,|IndentRow]|EncHead],
    Col = erlang:iolist_size(String)-2,
    [String|format_tail(Tail, Enc, State#{col := Col}, [], IndentRow)];
format_tail([Head|Tail], Enc, #{style := Style} = State, IndentAll, IndentRow) when Style =:= 'allman' ->  
    EncHead = Enc(Head, Enc, State),
    String = [$, |EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([Head|Tail], Enc, #{style := Style} = State, IndentAll, IndentRow) when Style =:= 'hortsmann' ->  
    EncHead = Enc(Head, Enc, State),
    String = [[IndentAll, $, | IndentAll]|EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([Head|Tail], Enc, #{style := Style} = State, IndentAll, IndentRow) when Style =:= 'otbs' ->  
    EncHead = Enc(Head, Enc, State),
    String = [$,|EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([Head|Tail], Enc, #{style := Style} = State, IndentAll, IndentRow) when Style =:= 'stroustrup' ->  
    EncHead = Enc(Head, Enc, State),
    String = [[IndentAll,$,]|EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([Head|Tail], Enc, #{style := Style} = State, IndentAll, IndentRow) when Style =:= 'whitesmiths';Style =:= 'gnu' ->  
    EncHead = Enc(Head, Enc, maps:update(level, (maps:get(level, State, 1) - 1), State)),
    String = [[$, | IndentAll]| EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([Head|Tail], Enc, #{style := _Style} = State, IndentAll, IndentRow) -> 
    EncHead = Enc(Head, Enc, State),
    String = [[$,|IndentAll]|EncHead],
    [String|format_tail(Tail, Enc, State, IndentAll, IndentRow)];
format_tail([], _, _, _, _) -> 
    [].


format_key_value_list(KVList, UserEnc, #{level := Level, style := _Style} = State) ->
    {_,Indent} = indent(State),
    NextState = State#{level := Level+1},
    {KISize, KeyIndent} = indent(NextState),
    EncKeyFun = fun(KeyVal, _Fun) -> UserEnc(KeyVal, UserEnc, NextState) end,
    EntryFun = fun({Key, Value}) ->
                       EncKey = key(Key, EncKeyFun),
                       ValState = NextState#{col := KISize + 2 + erlang:iolist_size(EncKey)},
                       [$, , KeyIndent, EncKey, ": " | UserEnc(Value, UserEnc, ValState)]
               end,
    format_object(lists:map(EntryFun, KVList), Indent, State).


format_object([], _, _State) -> <<"{}">>;
format_object([[_Comma,KeyIndent|Entry]], Indent, #{style := Style} = _State) -> % Top entry
    [_Key,_Colon|Value] = Entry,
    {_, Rest} = string:take(Value, [$\s,$\n]),
    [CP|_] = string:next_codepoint(Rest),
    if CP =:= ${ ->
            case Style of
                'whitesmiths' -> [ ${, KeyIndent, Entry, $\n, $}]  ;
                'hortsmann' -> [ Indent, ${, KeyIndent, Entry, $\n, $}]  ;
            _ ->  
                [ ${, KeyIndent, Entry, Indent, $}]
            end;
       CP =:= $[ ->
            case Style of 
                'allman' -> [Indent, ${, KeyIndent, Entry, Indent, $}]; 
                'hortsmann' -> [Indent, ${, KeyIndent, Entry, Indent, $}]; 
                'whitesmiths' -> [KeyIndent, ${, KeyIndent, Entry, KeyIndent, $}]; 
                _ -> [ ${, KeyIndent, Entry, Indent, $}]
            end;
       true ->
            ["{ ", Entry, " }"]
    end;
format_object([[_Comma,KeyIndent|Entry] | Rest], _Indent, #{style := Style, indent := _Ind} = _State) when Style =:= 'whitesmiths';Style =:= 'gnu' ->
    [${, KeyIndent, Entry, Rest, KeyIndent ,$}];
format_object([[_Comma,KeyIndent|Entry] | Rest], Indent, #{style := Style} = _State) when Style =:= 'allman' ->
    [Indent,${, KeyIndent, Entry, Rest, Indent,$}];
format_object([[_Comma,KeyIndent|Entry] | Rest], Indent, #{style := Style} = _State) when Style =:= 'hortsmann' ->
    [${, KeyIndent, Entry, Rest, Indent,$}];
format_object([[_Comma,KeyIndent|Entry] | Rest], Indent, #{style := _Style} = _State) ->
    [${, KeyIndent, Entry, Rest, Indent, $}].

indent(#{level := Level, indent := Indent}) ->
    Steps = Level * Indent,
    {Steps, steps(Steps)}.

steps(0)  -> ~"\n";
steps(2)  -> ~"\n  ";
steps(4)  -> ~"\n    ";
steps(6)  -> ~"\n      ";
steps(8)  -> ~"\n        ";
steps(10) -> ~"\n          ";
steps(12) -> ~"\n            ";
steps(14) -> ~"\n              ";
steps(16) -> ~"\n                ";
steps(18) -> ~"\n                  ";
steps(20) -> ~"\n                    ";
steps(N) ->  ["\n", lists:duplicate(N, " ")].

%% Dispatching any value through `Encode` could allow incorrect
%% JSON to be emitted (with keys not being strings). To avoid this,
%% the default encoder only supports binaries, atoms, and numbers.
%% Customisation is possible by overriding how maps are encoded in general.
key(Key, Encode) when is_binary(Key) -> Encode(Key, Encode);
key(Key, Encode) when is_atom(Key) -> Encode(atom_to_binary(Key, utf8), Encode);
key(Key, _Encode) when is_integer(Key) -> [$", encode_integer(Key), $"];
key(Key, _Encode) when is_list(Key) -> Key;
key(Key, _Encode) when is_float(Key) -> [$", encode_float(Key), $"].

encode_integer(Integer) -> integer_to_binary(Integer).

encode_float(Float) -> float_to_binary(Float, [short]).





