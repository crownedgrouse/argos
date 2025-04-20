
-module(argos_lib).

-export([options/1, get_decoders/1]).

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
            'map'       -> 'map';
            'proplist'  -> 'proplist' ;
            'record'    -> 'record' ;
            'struct'    -> 'struct';
            _           -> 'otp'
       end,
   B = case proplists:get_value(binary, O) of
            k -> k ;
            v -> v ;
            kv -> kv ;
            _ -> undefined

       end,
   U = case proplists:get_value(return, O) of
            tuple    -> tuple ;
            stack    -> stack ;
            _        -> undefined
       end,
   #opt{nl = N
      ,indent = I
      ,records = lists:flatten(R)
      ,mode = M
      ,binary = B
      ,aliases = lists:flatten(A)
      ,return = U
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

%% @doc Get decoders() map depending mode in config
%% @end
get_decoders(Opt) when is_record(Opt, opt)
    ->
        %erlang:display(Opt),
        case Opt#opt.mode of
            map      -> get_decs_mode(Opt);
            proplist -> get_decs_mode(Opt);
            record   -> get_decs_mode(Opt);
            struct   -> get_decs_mode(Opt);
            otp      -> get_decs_mode(Opt)
        end.

%% @doc Get decoder() map customized by config
%% @end
get_decs_mode(Opt)
    -> 
        #{array_start   => argos_dec_array_start_fun(Opt),
          array_push    => argos_dec_array_push_fun(Opt),
          array_finish  => argos_dec_array_finish_fun(Opt),
          object_start  => argos_dec_object_start_fun(Opt),
          object_push   => argos_dec_object_push_fun(Opt),
          object_finish => argos_dec_object_finish_fun(Opt),
          float         => argos_dec_from_binary_float_fun(Opt),
          integer       => argos_dec_from_binary_int_fun(Opt),
          string        => argos_dec_from_binary_str_fun(Opt),
          null          => argos_dec_null(Opt)}.

%% @doc 
%% @end
argos_dec_array_start_fun(_Opt)
    -> 
        fun(_) -> [] end.

argos_dec_array_push_fun(_Opt)
    ->  
        fun(Elem, Acc) -> [Elem | Acc] end.

argos_dec_array_finish_fun(_Opt)
    ->
        fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end.

argos_dec_object_start_fun(_Opt)
    ->
        fun(_) -> [] end.

argos_dec_object_push_fun(Opt)
    ->
        case Opt#opt.binary of
            'k' when Opt#opt.mode =/= otp 
                -> fun(Key, Value, Acc) -> 
                        case Value of
                            Value when is_list(Value) 
                                ->  Value2 = lists:flatmap(fun(X)-> 
                                                                case X of 
                                                                    X when is_binary(X)
                                                                        -> [binary:bin_to_list(X)];
                                                                    X -> [X]
                                                                end
                                                           end, Value),
                                    [{Key, Value2} | Acc] ;
                            Value when is_binary(Value) 
                                -> [{Key, binary:bin_to_list(Value)} | Acc] ;
                            _   -> [{Key, Value} | Acc] 
                        end
                   end;
            'v' when Opt#opt.mode =/= otp 
                -> fun(Key, Value, Acc) ->  [{binary:bin_to_list(Key), Value} | Acc] end;
            _   -> fun(Key, Value, Acc) -> [{Key, Value} | Acc] end
        end.

argos_dec_object_finish_fun(Opt)
    -> 
        case Opt#opt.mode of
            'struct' 
                ->  fun(Acc, OldAcc) -> {Acc, OldAcc} end ;
            _   -> % Legacy
                    fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end
        end.

argos_dec_from_binary_float_fun(_Opt)
    -> 
        fun erlang:binary_to_float/1.

argos_dec_from_binary_int_fun(_Opt)
    -> 
        fun erlang:binary_to_integer/1.

argos_dec_from_binary_str_fun(_Opt)
    ->
       fun (Value) -> Value end.

argos_dec_null(_Opt)
    ->
        null.