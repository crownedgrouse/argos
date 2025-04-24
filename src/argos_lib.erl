
-module(argos_lib).

-export([options/1, get_decoders/1,valid_to_file/1]).

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
   T = case proplists:get_value(to, O) of
            Z when is_list(Z) -> Z ;
            _        -> []
       end,
   #opt{nl = N
      ,indent = I
      ,records = lists:flatten(R)
      ,mode = M
      ,binary = B
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
    case Opt#opt.mode of
        record -> 
            fun(Key, Value, Acc) -> [{Key, Value} | Acc] end ;
        otp -> fun(Key, Value, Acc) -> [{Key, Value} | Acc] end ;
        _ -> % other modes
            case Opt#opt.binary of
                'k' 
                    -> fun(Key, Value, Acc) -> 
                            [{Key, smart_binary_to_list_value(Value)} | Acc] 
                       end;
                'v' 
                    -> fun(Key, Value, Acc) -> [{binary:bin_to_list(Key), Value} | Acc] end;
                undefined 
                    -> fun(Key, Value, Acc) -> [{binary:bin_to_list(Key), smart_binary_to_list_value(Value)} | Acc] end
            end
    end.

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
    ->   V;
smart_binary_to_list_value(V)  
    ->   V.

argos_dec_object_finish_fun(Opt)
    -> 
    case Opt#opt.mode of
        record -> fun(Acc, OldAcc) -> 
            {recordify(lists:reverse(Acc), Opt), OldAcc} end ;
        _ ->  % other modes
        case Opt#opt.mode of
            'struct' 
                ->  fun(Acc, OldAcc) -> {Acc, OldAcc} end ;
            _   -> % Legacy
                    fun(Acc, OldAcc) -> 
                        {maps:from_list(Acc),OldAcc}  end
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
%% @doc Cast data (list if printable, otherwise binary)
%% @end
-spec cast(any()) -> any().

cast(V) -> cast(V, binary).

cast(V, Opt) when is_record(Opt, opt) ->
   case Opt#opt.binary of
      v  -> cast(V, binary);
      kv -> cast(V, binary);
      _  -> cast(V, undefined)
   end;

cast(V, binary) when is_binary(V) -> V ;

cast(V, binary) when is_list(V) -> erlang:list_to_binary(V);

cast(V, _) when is_binary(V) ->
   X = erlang:binary_to_list(V),
   case io_lib:printable_unicode_list(X) of
      true  -> X;
      false -> V
   end;
cast(V, _) when is_list(V)   ->
   case io_lib:printable_unicode_list(V) of
      false -> lists:flatmap(fun(Z) -> [cast(Z)] end, V);
      true  -> V
   end;
cast(V, _) -> V .

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

%%==============================================================================
%% @doc Safe list to atom (check > 255 of UTF8)
%% @end
% -spec safe_list_to_atom(list()) -> atom() | binary().

% safe_list_to_atom(L) ->
%    R = case get(jason_binary) of
%             k  -> list_to_binary(L);
%             kv -> list_to_binary(L);
%             _  ->  case catch list_to_atom(L) of
%                         {'EXIT', _} -> list_to_binary(L);
%                         X -> X
%                      end
%          end,
%    R.

%% RECORDS %%
%%==============================================================================
%% @doc Translate to record
%% @end
-spec recordify(list(), tuple()) -> tuple().

recordify(Obj, Opt)
   when is_list(Obj)
   -> % Replace binary keys by atom key, and detect values types
      R = lists:flatmap(fun({K, V}) -> [{erlang:binary_to_atom(K, utf8), cast(V)}] end, Obj),
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
                                       false -> create_module(Aliase, T, true) ;
                                       _     -> ok
                                    end
                     end,
                     Aliase;
               '' -> HH = list_to_atom(integer_to_list(erlang:phash2(T))),

                     case lists:any(fun(X) -> case X of HH -> true; _ -> false end end, get(argos_adhoc)) of
                           true  -> ok ;
                           false -> create_module(HH, T, false)
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
-spec create_module(atom(), list(), atom()) -> atom().

create_module(H, T, Mode) ->
  %erlang:display({H, T, Mode}),
  % Module declaration
  M1 = parse_forms(io_lib:format("-module(~p).~n", [H])),
  {Ks, _Ts} = lists:unzip(T),
  M10 = parse_forms(io_lib:format("-argos(argonaut).~n", [])),

  % Functions export
  M2 = parse_forms(io_lib:format("-export([new/0, fields/0, size/0, def/0, ~ts]).~n",
                    [string:join(lists:flatmap(fun(K) -> [io_lib:format("~p/1,~p/2", [K,K])] end, Ks), ", ")])),

  % Json types definition
  M3  = parse_forms(io_lib:format("-type literal() :: null | true | false .~n",[])),
  M31 = parse_forms(io_lib:format("-type datetime() :: calendar:datetime() .~n",[])),

  % Record definition
  DefT = string:join(lists:flatmap(fun({K, V}) ->
                       Def1  =    case V of
                                      {record, R} -> io_lib:format(" = ~p:new() ", [R]) ;
                                      integer -> " = 0 " ;
                                      float   -> " = 0.0 " ;
                                      list    -> " = [] " ;
                                      literal -> " = null ";
                                      binary  -> " = <<\"\">>";
                                      datetime-> " = {{1970,1,1},{0,0,0}}"
                                 end,
                       Type1 =    case V of
                                      {record, A} -> "'" ++  atom_to_list(A) ++ "':'" ++ atom_to_list(A) ++ "'" ;
                                      V when is_atom(V) -> atom_to_list(V)
                                 end,
                       [io_lib:format("~p ~s :: ~s()", [K, Def1, Type1])]
                                               end, T), ", "),
  M40 = parse_forms(io_lib:format("-record(~p, {~ts}).~n", [H, DefT])),

  M41 = parse_forms(io_lib:format("-opaque ~p() :: #~p{}.~n", [H, H])),
  M42 = parse_forms(io_lib:format("-export_type([~p/0]).~n", [H])),

  % Function definitions
  M50 = parse_forms(io_lib:format("new() -> #~p{}.~n", [H])),
  M51 = parse_forms(io_lib:format("fields() -> record_info(fields, ~p).~n", [H])),
  M52 = parse_forms(io_lib:format("size()   -> record_info(size, ~p).~n", [H])),

  RecDef = io_lib:format("-record(~p, {~ts}).~n", [H, DefT]),
  M53 = parse_forms(io_lib:format("def() -> \"-record(~p, {~ts}).\".~n", [H, DefT])),

  M54 = lists:flatmap(fun({K, Type}) ->
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
  Binary = case compile:forms(lists:flatten([M1,M10,M2,M3,M31,M40,M41,M42,M50,M51,M52,M53,M54]),[debug_info]) of
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
          File when is_list(File)     -> append_file(File, RecDef);
          _ -> ok
      end,

  % Load module
  Target = case Mode of
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

valid_to_file(To) when is_list(To) ->
   case filelib:is_file(To) of
      false -> case filelib:is_dir(filename:dirname(To)) of
                  true  -> true ;
                  false -> false
               end;
      true  -> case filelib:is_regular(To) of
                  true -> case filelib:file_size(To) of
                              0 -> true ;
                              _ -> notempty
                           end
               end
   end;

valid_to_file(_) -> skip.