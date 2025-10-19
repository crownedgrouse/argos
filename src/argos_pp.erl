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
    FOpt = #{indent => IndLen},
    io:put_chars(json:format(Term, Fun, FOpt)).


formatter(Other, Encode, State) -> json:format_value(Other, Encode, State).
