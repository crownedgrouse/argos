
-module(argos_lib).

-export([options/1]).

-include("argos.hrl").

%%==============================================================================

%% @doc Analyze options
%% @end
-spec options(list()) -> tuple().

options(O) ->
   I = case proplists:get_value(indent, O) of
            undefined -> "" ;
            false -> "" ;
            true  -> "   "
       end,
   N = case proplists:get_value(indent, O) of
            undefined -> "" ;
            false -> "" ;
            true  -> "\n"
       end,
   A = case proplists:get_value(aliases, O) of
            undefined -> [] ;
            Y  when is_atom(Y)   -> get_records([Y]);
            Y  when is_tuple(Y) -> get_records([Y]);
            Y  when is_list(Y)  -> get_records(Y);
            _  -> throw({0, {invalid, records}})
       end,
   R = case proplists:get_value(records, O) of
            undefined -> [] ;
            X  when is_atom(X)   -> get_records([X]);
            X  when is_tuple(X) -> get_records([X]);
            X  when is_list(X)  -> get_records(X);
            _  -> throw({0, {invalid, records}})
       end,
   M = case proplists:get_value(mode, O) of
            map      -> map ;
            proplist -> proplist ;
            'record' -> 'record' ;
            _        -> struct
       end,
   B = case proplists:get_value(binary, O) of
            k -> k ;
            v -> v ;
            kv -> kv ;
            _ -> undefined

       end,
   #opt{nl = N
      ,indent = I
      ,records = lists:flatten(R)
      ,mode = M
      ,binary = B
      ,aliases = lists:flatten(A)
      }.


%% @doc Get records info
%% @end
-spec get_records(any()) -> list() | no_return().

get_records(X) when is_list(X)
    -> 
        lists:flatmap(fun(E) -> [get_records(E)] end, X) ;

get_records(X) 
    when is_tuple(X),
         is_atom(element(1, X)),
         is_list(element(2, X))
    -> 
       % Check all entries are atoms
       case lists:all(fun(A) -> case is_atom(A) of true -> true ; _ -> false end end, element(2, X)) of
           true  -> X ;
           false -> throw({0, {invalid, records}}),
                    []
       end;

get_records(X) when is_atom(X)
    -> 
       % Extract record info from pid
       Beam = case code:which(X) of
                 cover_compiled -> "";
                 preloaded      -> "";
                 non_existing   -> "";
                 B              -> B
              end,
       case beam_lib:chunks( Beam,[abstract_code]) of
           {ok,{_, [{abstract_code, {_, Abs }}]}}
               -> extract_records_ac(Abs);
           _   -> throw( {0, {invalid, abstract_code}})

       end;

get_records(_) -> throw( {0, {invalid, records}}).


%% @doc Extract records in abstract code
%% @end
-spec extract_records_ac(list()) -> list().

%   [{attribute,1,file,{"src/jason_pp.erl",1}},
%   {attribute,26,module,jason_pp},
%   {attribute,28,export,[{indent,1},{indent,2}]},
%   {attribute,30,record,
%         {pp,[{record_field,30,{atom,30,style},{atom,30,ratliff}},
%            {record_field,31,{atom,31,depth},{integer,31,0}},
%            {record_field,32,{atom,32,nl},{string,32,[...]}},
%            {record_field,33,{atom,33,tab},{string,33,...}},
%            {record_field,34,{atom,34,...},{atom,...}}]}},
%   {function,38,indent,1,

extract_records_ac(Abs) 
    ->
        Raw = lists:filter(fun(E) ->  
            case (is_tuple(E) and (size(E) > 2)) of
                 true -> case element(3, E) of
                              record -> true ;
                              _ -> false
                         end;
                  false -> false
             end
         end, Abs),
   lists:flatmap(fun({attribute, _, record, {N, FF}}) 
    ->
        F = lists:flatmap(fun(R) -> 
            case element(1, R) of
                record_field -> 
                    {atom, _, RA} = element(3, R),
                    [RA];
                typed_record_field -> 
                    {typed_record_field, W, _} = R,
                    {atom, _, RT} = element(3, W),
                                    [RT]
            end
            end, FF), [{N, F}]
        end, Raw).