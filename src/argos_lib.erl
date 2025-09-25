
-module(argos_lib).

-export([options/1, get_decoders/1,valid_to_file/1, append_file/2]).

-include("argos.hrl").

%%==============================================================================

%% @doc Analyze options
%% @end
-spec options(list()) -> tuple().

options(O0) ->
   O = case proplists:get_value(kv, O0) of
            atom -> O0 ++ [{k, atom}, {v, atom}] ;
            string -> O0 ++ [{k, string}, {v, string}] ;
            binary ->  O0 ++ [{k, binary}, {v, binary}];
            _ -> O0
       end,
   %
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
            'm'         -> 'map';
            'proplist'  -> 'proplist' ;
            'proplists' -> 'proplist' ;
            'p'         -> 'proplist' ;
            'record'    -> 'record' ;
            'records'   -> 'record' ;
            'r'         -> 'record' ;
            'struct'    -> 'struct' ;
            's'         -> 'struct' ;
            'graphql'   -> 'graphql' ;
            'gql'       -> 'graphql' ;
            'g'         -> 'graphql' ;
            _           -> 'otp'
       end,
   K = case proplists:get_value(k, O) of
            atom -> atom ;
            string -> string ;
            binary -> binary ;
            _ -> undefined
       end,
   V = case proplists:get_value(v, O) of
            atom -> atom ;
            string -> string ;
            binary -> binary ;
            _ -> undefined
       end,
   U = case proplists:get_value(return, O) of
            tuple    -> tuple ;
            stack    -> stack ;
            _        -> undefined
       end,
   T = case proplists:get_value(to, O) of
            Z when is_list(Z) -> Z ;
            _        -> []
       end,
   #opt{nl = N
      ,indent = I
      ,records = lists:flatten(R)
      ,mode = M
      ,k = K
      ,v = V
      ,aliases = lists:flatten(A)
      ,return = U
      ,to = T
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

%   [{attribute,1,file,{"src/argos_pp.erl",1}},
%   {attribute,26,module,argos_pp},
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
            otp      -> get_decs_mode(Opt);
            graphql  -> get_decs_mode(Opt)
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

argos_dec_array_push_fun(Opt)
    ->  
        case Opt#opt.v of
            undefined ->  fun(Elem, Acc) -> [Elem | Acc] end;
            _         ->  fun(Elem, Acc) -> [format(Elem, Opt#opt.v, false) | Acc] end
        end.

argos_dec_array_finish_fun(_Opt)
    ->
        fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end.

argos_dec_object_start_fun(_Opt)
    ->
    fun(_) -> [] end.

argos_dec_object_push_fun(Opt)
    ->
    case Opt#opt.mode of
        otp -> fun(Key, Value, Acc) -> [{Key, Value} | Acc] end ;
        proplist -> % struct with atom key and value as string by default
            V = case Opt#opt.v of
                undefined -> string ;
                X -> X
                end,
            fun(Key, Value, Acc) -> 
                [{format(Key, atom, true), format(Value, V, false)} | Acc] 
            end;
        map -> % map with atom key and value as string by default
            K = case Opt#opt.k of
                undefined -> atom ;
                Y -> Y
                end,
            V = case Opt#opt.v of
                undefined -> string ;
                X -> X
                end,
            fun(Key, Value, Acc) -> 
                [{format(Key, K, true), format(Value, V, false)} | Acc] 
            end;
        record -> 
            V = case Opt#opt.v of
                undefined -> string ;
                X -> X
                end,
            fun(Key, Value, Acc) -> [{format(Key, atom, true), format(Value, V, false)} | Acc] end ;
        _ -> % other modes
            fun(Key, Value, Acc) -> 
                [{format(Key, Opt#opt.k, false), format(Value, Opt#opt.v, false)} | Acc] 
            end
    end.

%% @doc 
%% @end
format(X, atom, Fatal) when is_binary(X) -> 
    safe_list_to_atom(smart_binary_to_list_value(X), Fatal);
format(X, string, _)  -> 
    smart_binary_to_list_value(X);
format(X, _, _) -> 
    X.

%% @doc 
%% @end
smart_binary_to_list_value(V) when is_list(V)
    -> 
    lists:flatmap(fun(X)-> 
                    case X of 
                        X when is_binary(X)
                            -> [binary:bin_to_list(X)];
                        X -> [X]
                    end
               end, V);
smart_binary_to_list_value(V) when is_binary(V)       
    ->   
    binary:bin_to_list(V);
smart_binary_to_list_value(V)  
    -> V.

%% @doc 
%% @end
-spec safe_list_to_atom(list(), boolean()) -> atom() | binary().

safe_list_to_atom(L, Fatal) ->
   try
        list_to_atom(L)
   catch
       _:_:_ -> case Fatal of
                false -> list_to_binary(L);
                true  -> error(invalid_atom_key)
            end
   end.

argos_dec_object_finish_fun(Opt)
    -> 
    case Opt#opt.mode of
        record ->  
            fun(Acc, OldAcc) -> 
                {recordify(lists:reverse(Acc), Opt), OldAcc} 
            end ;
        graphql ->  
            fun(Acc, OldAcc) -> 
                {recordify(lists:reverse(Acc), Opt), OldAcc} 
            end ;
        proplist ->  
            fun(Acc, OldAcc) -> 
                {lists:reverse(Acc), OldAcc} 
            end ;
        struct ->  
            fun(Acc, OldAcc) -> 
                {lists:reverse(Acc), OldAcc} 
            end ;
        _ ->  % other modes
            fun(Acc, OldAcc) -> 
                {maps:from_list(Acc),OldAcc}  
            end
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

%% General %%
%%==============================================================================
%% @doc Append data to file
%% @end
-spec append_file(list(), any()) -> atom().

append_file(Filename, Bytes)
    when is_list(Filename) ->
    case file:open(Filename, [append]) of
        {ok, IoDevice} ->
            ok = file:write(IoDevice, Bytes),
            file:close(IoDevice);
        {error, Reason} ->
            io:format("~s open error  reason:~s~n", [Filename, Reason])
    end.

%% RECORDS %%
%%==============================================================================
%% @doc Translate to record
%% @end
-spec recordify(list(), tuple()) -> tuple().

recordify(Obj, Opt)
   when is_list(Obj)
   -> % Replace binary keys by atom key, and detect values types
    R = lists:flatmap(fun({K, V}) -> case K of K when is_atom(K) -> [{K, V}] ; _ -> [{erlang:binary_to_atom(K, utf8), V}] end end, Obj),
    T = lists:flatmap(fun({K, V}) -> [{K, detect_type(V)}] end, R),
    CR = case Opt#opt.records of
               [] -> '' ;
               undefined -> '' ;
               X  -> % Some records announced, check if we find it
                     Keys = lists:flatmap(fun({K, _}) -> [K] end, R),
                     case lists:keyfind(Keys, 2, X) of
                           false -> '' ;
                           {F, _}     -> F
                     end
           end,
    AR = case Opt#opt.aliases of
               [] -> '' ;
               undefined -> '' ;
               Y  -> % Some records announced, check if we find it
                     Keys2 = lists:flatmap(fun({K, _}) -> [K] end, R),
                     case lists:keyfind(Keys2, 2, Y) of
                           false -> '' ;
                           {F2, _}     -> {alias, F2}
                     end
           end,
    CRX = case AR of
              '' -> case CR of
                       '' -> '' ;
                       _  -> CR
                    end;
              _ -> AR
            end,
      % Create module for this record handling if not existing
      case get(argos_adhoc) of
            undefined -> put(argos_adhoc, []);
            _         -> ok
      end,
      % Hash Erlang term for ad hoc record name if necessary otherwise use record name detected
      H = case CRX of
               {alias, Aliase} ->
                     case lists:any(fun(X) -> case X of Aliase -> true; _ -> false end end, get(argos_adhoc)) of
                           true  -> ok ;
                           false -> % Check if module is already loaded from a former dump on disk
                                    case code:is_loaded(Aliase) of
                                       false -> create_module(Aliase, T, true, Opt#opt.mode) ;
                                       _     -> ok
                                    end
                     end,
                     Aliase;
               '' -> HH = list_to_atom(integer_to_list(erlang:phash2(T))),

                     case lists:any(fun(X) -> case X of HH -> true; _ -> false end end, get(argos_adhoc)) of
                           true  -> ok ;
                           false -> create_module(HH, T, false, Opt#opt.mode)
                     end,
                     HH;
               RN -> RN
          end,
      % Create record
      V = lists:flatmap(fun({_, Z}) -> [Z] end, R),
      erlang:list_to_tuple([H] ++ V).

%%==============================================================================
%% @doc Detect type of data
%% @end
-spec detect_type(any()) -> atom() | tuple().

detect_type(V) when is_atom(V)    -> literal ;
detect_type(V) when is_float(V)   -> float ;
detect_type(V) when is_integer(V) -> integer ;
detect_type(V) when is_list(V)    -> list;
detect_type(V) when is_binary(V)  -> binary;
detect_type({K,V}) when is_tuple(K),is_tuple(V)
                                  -> datetime;
detect_type(V) when is_tuple(V)   -> {record, element(1, V)}.

%%==============================================================================
%% @doc Create argonaut module for record handling
%% @end
-spec create_module(atom(), list(), boolean(), atom()) -> atom().

create_module(H, T, Aliase, Mode) ->
  % Module declaration
  M1 = parse_forms(io_lib:format("-module(~p).~n", [H])),
  {Ks, _Ts} = lists:unzip(T),
  M10 = parse_forms(io_lib:format("-argos(argonaut).~n", [])),

  % Functions export
  M2 = parse_forms(io_lib:format("-export([new/0, fields/0, size/0, def/0, schema/0, ~ts]).~n",
                    [string:join(lists:flatmap(fun(K) -> [io_lib:format("~p/1,~p/2", [K,K])] end, Ks), ", ")])),

  % Json types definition
  M3  = parse_forms(io_lib:format("-type literal() :: null | true | false .~n",[])),
  M31 = parse_forms(io_lib:format("-type datetime() :: calendar:datetime() .~n",[])),

  % Record definition
  DefT = string:join(lists:flatmap(fun({K, V}) ->
  Def1 = case V of
              {record, R} -> io_lib:format(" = ~p:new() ", [R]) ;
              integer -> " = 0 " ;
              float   -> " = 0.0 " ;
              list    -> " = [] " ;
              literal -> " = null ";
              binary  -> " = <<>>";
              datetime-> " = {{1970,1,1},{0,0,0}}"
            end,
    Type1 = case V of
                {record, A} -> "'" ++  atom_to_list(A) ++ "':'" ++ atom_to_list(A) ++ "'" ;
                V when is_atom(V) -> atom_to_list(V)
            end,
    [io_lib:format("~p ~s :: ~s()", [K, Def1, Type1])]
    end, T), ", "),
    % GraphQL schema definition
    DefS = string:join(lists:flatmap(fun({K, V}) ->
    Type2 = case V of
                {record, R} -> valid_gql_name(R) ;
                integer -> "Int" ;
                float   -> "Float" ;
                list when K=="id";K=="ID";K=="Id"
                        -> "ID";
                list    -> "String" ;
                literal -> "Boolean";
                binary  -> "String";
                datetime-> "Datetime"
           end,
     [io_lib:format(" ~ts: ~ts",[valid_gql_name(K), Type2])]          
    end, T), "\n"),

  M40 = parse_forms(io_lib:format("-record(~p, {~ts}).~n", [H, DefT])),

  M41 = parse_forms(io_lib:format("-opaque ~p() :: #~p{}.~n", [H, H])),
  M42 = parse_forms(io_lib:format("-export_type([~p/0]).~n", [H])),

  % Function definitions
  M50 = parse_forms(io_lib:format("new() -> #~p{}.~n", [H])),
  M51 = parse_forms(io_lib:format("fields() -> record_info(fields, ~p).~n", [H])),
  M52 = parse_forms(io_lib:format("size()   -> record_info(size, ~p).~n", [H])),
  M53 = parse_forms(io_lib:format("schema() -> ~p.~n",[DefS])),

  RecDef = io_lib:format("-record(~p, {~ts}).~n", [H, DefT]),
  GqlDef_ = io_lib:format("~ntype ~ts {~n~ts~n}~n",[valid_gql_name(H), DefS]),
  % If an ID/id is detected, add automatically a Query type
  GqlDef = case lists:filter(fun(X) -> case X of X when X=='id';X=='ID';X=='Id' -> true; X -> false end end, Ks) of
                [] -> GqlDef_ ;
                XL -> GqlDef_ ++ 
                      string:join(
                        lists:flatmap( 
                            fun(Z) -> [io_lib:format("~ntype Query {~n ~ts(~ts: ID!): ~ts~n}~n", [valid_gql_name(H), Z, valid_gql_name(H)])]
                            end, XL)
                      , "\n")
           end,

  M54 = parse_forms(io_lib:format("def() -> \"-record(~p, {~ts}).\".~n", [H, DefT])),

  M55 = lists:flatmap(fun({K, Type}) ->
                       G = case Type of
                                      {record, R} -> io_lib:format(",is_tuple(V),(~p == element(1, V)) ", [R]) ;
                                      integer -> ",is_integer(V) " ;
                                      float   -> ",is_float(V) " ;
                                      list    -> ",is_list(V) " ;
                                      binary  -> ",is_binary(V) " ;
                                      literal -> ",is_atom(V),((V == 'true') or (V == 'false') or (V == 'null')) ";
                                      datetime-> ",is_tuple(V) "
                           end,
                       [parse_forms(io_lib:format("~p(#~p{~p = X}) -> X.~n", [K, H, K])),
                        parse_forms(io_lib:format("~p(R, V) when is_record(R, ~p)~s -> R#~p{~p = V}.~n",
                                                  [K, H, G, H, K]))] end, T),
  % Compile forms
  Binary = case compile:forms(lists:flatten([M1,M10,M2,M3,M31,M40,M41,M42,M50,M51,M52,M53,M54,M55]),[debug_info]) of
              {ok, _, B} -> B ;
              {ok, _, B, Warnings} -> io:format("Warning : ~p~n", [Warnings]), B ;
              error -> io:format("Error while compiling : ~p~n", [H]),
                       <<"">>;
              {error,Errors,Warnings} -> io:format("Error   : ~p~n", [Errors]),
                                         io:format("Warning : ~p~n", [Warnings]),
                                         <<"">>
           end,
  % Dump record def if requested
  _ = case get(argos_to) of
          undefined -> ok ;
          File when is_list(File), Mode=='graphql' ->
                append_file(File, GqlDef);
          File when is_list(File), Mode=='record' ->
                append_file(File, RecDef);
          _ -> ok
      end,

  % Load module
  Target = case Aliase of
     true -> % Using aliases need to set a valid path in order to be able to dump them elsewhere
           Dir = code:priv_dir(argos),
           Dir1 = filename:join([Dir, "dump", atom_to_list(H)]),
           ok = filelib:ensure_dir(filename:join([Dir1, "fakedir"])),
           T1 = filename:join([Dir1, atom_to_list(H)++".beam"]),
           ok = file:write_file(T1, Binary),
           code:replace_path(H, Dir1),
           T1;
     _ -> atom_to_list(H)
  end,
  case code:load_binary(H, Target, Binary) of
     {module, _}    -> put(argos_adhoc, lists:flatten(get(argos_adhoc) ++ [H])) ;
     {error, _What} -> ok
  end.

%% @doc Fix invalid name from GraphQL perspective
%% @end

valid_gql_name(N) when is_atom(N)->
    valid_gql_name(atom_to_list(N));
valid_gql_name(N) when is_list(N)->
    % Name must start with a Letter or underscode, then followed by Letter, Digit or Undescore
    case re:run(N,"^[A-Za-z_]{1}[A-Za-z0-9_]{0,}$") of
        {match, _} -> N;
        nomatch -> % Invalid
            % Test some way to fix
            case re:run(N,"^.[A-Za-z0-9_]{0,}$") of
                {match, _} -> 
                    valid_gql_name("_"++N);
                nomatch    -> 
                    valid_gql_name(re:replace(N,"[^A-Za-z0-9_]", "_",[global, {return, list}]))
            end
    end.

%%==============================================================================
%% @doc Parse forms
%% @end
-spec parse_forms(list()) -> atom() | list().

parse_forms(C) ->
         Code = lists:flatten(C),
         case erl_scan:string(Code) of
              {ok, S, _} ->  case erl_parse:parse_form(S) of
                                 {ok, PF}    -> PF ;
                                 {error, Ei} -> erlang:display({parse_error, Ei, io_lib:format("~ts",[Code])}), false
                             end;
              {error, EI, EL} -> erlang:display({scan_error, EI, EL, io_lib:format("~ts",[Code])}), false
         end.

%%==============================================================================
%% @doc Check if target file is valid (no exist or empty), upper dir. exists
%% @end
-spec valid_to_file(list()) -> atom().

valid_to_file([]) -> skip;
valid_to_file(To) when is_list(To) ->
   case file:read_file_info(To) of
     {error, Reason} when Reason =/= enoent -> {error, Reason};
     _ -> 
           case filelib:is_file(To) of
              false -> case filelib:is_dir(filename:dirname(To)) of
                          true  -> true ;
                          false -> {error, #{code => enoent, errmsg=>"Upper directory to found"}}
                       end;
              true  -> case filelib:is_regular(To) of
                          true -> case filelib:file_size(To) of
                                      0 -> true ;
                                      _ -> {error, #{code => efbig, errmsg => "File must be empty if already existing"}}
                                   end;
                          false -> {error, #{code => einval, errmsg=>"Not a regular file"}}
                       end
           end
    end;

valid_to_file(_) -> skip.