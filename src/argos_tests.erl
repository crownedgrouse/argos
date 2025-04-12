-module(argos_tests).

-include_lib("eunit/include/eunit.hrl").

long_key()-> string:copies("a", 260).


argos_decode_options_test() ->
     ?assertEqual([{<<"ab">>,<<"cd">>}], argos:decode('{"ab": "cd"}'))
     ,?assertEqual({ok,[{<<"ab">>,<<"cd">>}]}, argos:decode('{"ab": "cd"}', [{return, tuple}]))
     ,?assertMatch({1, _}, catch argos:decode('{"ab": "cd"'))
     ,?assertMatch({error,{1,_}}, argos:decode('{"ab": "cd"', [{return, tuple}]))
     ,?assertEqual(enoent, catch argos:decode_file("x"))
     ,?assertEqual({error, enoent}, argos:decode_file("x", [{return, tuple}]))
     .

argos_decode_types_test() ->
%% Atoms
% null                 -> null
?assertEqual(null, argos:decode("null"))
% true                 -> true
,?assertEqual(true, argos:decode("true"))
% false                -> false
,?assertEqual(false, argos:decode("false"))
% "any"                -> <<"any">>
,?assertEqual(<<"any">>, argos:decode('"any"'))

%% Integer
% 123                  -> 123
,?assertEqual(123, argos:decode("123"))

%% Float (Automatic precision)
% 123.456789           -> 123.456789
,?assertEqual(123.456789, argos:decode("123.456789"))
% 2.3                  -> 2.3
,?assertEqual(2.3, argos:decode("2.3"))
% 2300.0               -> 2.3e3
,?assertEqual(2.3e3, argos:decode("2300.0"))
% 0.0023               -> 0.0023
,?assertEqual(0.0023, argos:decode("0.0023"))

%% List
% [1,2,3]              -> [1,2,3]
,?assertEqual([1,2,3], argos:decode("[1,2,3]"))
% ["a","b","c"]        -> [<<"a">>,<<"b">>,<<"c">>]
,?assertEqual([<<"a">>,<<"b">>,<<"c">>], argos:decode('["a","b","c"]'))

%% Dates
%% {{1970,1,1},{0,0,0}}    -> "1970-01-01T00:00:00Z"
,?assertEqual({{1970,1,1},{0,0,0}}, argos:decode("\"1970-01-01T00:00:00Z\""))
%% {{1970,1,1},{0,0,0}}    -> "1970-01-01T00:00:00.000Z"
,?assertEqual({{1970,1,1},{0,0,0.0}}, argos:decode("\"1970-01-01T00:00:00.000Z\""))


%% Binary (key/value) mode=struct (default)
% "abc"                -> <<"abc">>
,?assertEqual(<<"abc">>, argos:decode('"abc"'))

%% Struct
%  mode=struct (default)
% {"abc": "def"}       -> [{<<"abc">>,<<"def">>}]
,?assertEqual([{<<"abc">>,<<"def">>}], argos:decode('{"abc": "def"}'))
,?assertEqual([{<<"abc">>,<<"def">>}], argos:decode('{"abc": "def"}', [{mode, struct}]))
,?assertEqual([{<<"abc">>,<<"def">>}], argos:decode('{"abc": "def"}', [{mode, whatever}]))
%  mode=proplist
% {"abc": "def"}       -> [{abc,"def"}]
,?assertEqual([{abc,"def"}], argos:decode('{"abc": "def"}', [{mode, proplist}]))
%  mode=map
% {"abc": "def"}       -> #{abc => "def"}
,?assertEqual(#{abc => "def"}, argos:decode('{"abc": "def"}', [{mode, map}]))
%  mode=record
% {"abc": "def"}       -> {'111259705',"def"}
,?assertEqual({'111259705',"def"}, argos:decode('{"abc": "def"}', [{mode, record}]))


%% Record -
% {"k1": 1,"k2": "ab"} -> [{<<"k1">>,1}, {<<"k2">>,<<"ab">>}]
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], argos:decode('{"k1": 1,"k2": "ab"}'))
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], argos:decode('{"k1": 1,"k2": "ab"}', [{mode, struct}]))
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], argos:decode('{"k1": 1,"k2": "ab"}', [{mode, whatever}]))
%  mode=proplist
% {"k1": 1,"k2": "ab"} -> [{k1,1},{k2,"ab"}]
,?assertEqual([{k1,1},{k2,"ab"}], argos:decode('{"k1": 1,"k2": "ab"}', [{mode, proplist}]))
%  mode=map
% {"k1": 1,"k2": "ab"} -> #{k1 => 1,k2 => "ab"}
,?assertEqual(#{k1 => 1,k2 => "ab"}, argos:decode('{"k1": 1,"k2": "ab"}', [{mode, map}]))
%  mode=record
% {"k1": 1,"k2": "ab"} -> {'8056669',1,"ab"}
,?assertEqual({'8056669',1,"ab"}, argos:decode('{"k1": 1,"k2": "ab"}', [{mode, record}]))

%%% Special cases
% Atom > 255 characters kept binary
%  mode=proplist
,?assertEqual([{list_to_binary(long_key()),"x"}], argos:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, proplist}]))
%  mode=map
,?assertEqual(#{list_to_binary(long_key()) => "x"}, argos:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, map}]))
%  mode=record
,?assertEqual(system_limit, catch argos:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, record}]))

% Atom with UTF-8 should be well handled

.

argos_decode_literals_test() ->
     %% Literals
     ?assertEqual({ok, false}, argos:decode(<<"false">>, [{return, tuple}])),
     ?assertEqual({ok, true}, argos:decode(<<"true">>, [{return, tuple}])),
     ?assertEqual({ok, null} , argos:decode(<<"null">>, [{return, tuple}])).

argos_decode_numbers_test() ->
     %% Numbers: Integer
     % positive integer
      ?assertEqual({ok, 1}, argos:decode(<<"1">>, [{return, tuple}])),
     % zero
      ?assertEqual({ok, 0}, argos:decode(<<"0">>, [{return, tuple}])),
     % negative integer
      ?assertEqual({ok, -1}, argos:decode(<<"-1">>, [{return, tuple}])),
     % large integer (no limit on size)
      ?assertEqual({ok, 111111111111111111111111111111111111111111111111111111111111111111111111111111},
        argos:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>, [{return, tuple}])),
     % integer with leading zero
      ?assertEqual({ok, 0} , argos:decode(<<"00">>, [{return, tuple}])),
      ?assertEqual({ok, 1}, argos:decode(<<"01">>, [{return, tuple}])),
      ?assertEqual({ok, -1}, argos:decode(<<"-01">>, [{return, tuple}])),
     % integer can't begin with an explicit plus sign",
      ?assertMatch({error, _}, argos:decode(<<"+1">>, [{return, tuple}])),

     %% Numbers: Floats
     % float: decimal notation",
      ?assertEqual({ok, 1.23}, argos:decode(<<"1.23">>, [{return, tuple}])),
      ?assertEqual({ok, 1.23456789}, argos:decode(<<"1.23456789">>, [{return, tuple}])),
     % float: exponential notation",
      ?assertEqual({ok, 12.345}, argos:decode(<<"12345e-3">>, [{return, tuple}])), % lower case 'e'
      ?assertEqual({ok, 12.345}, argos:decode(<<"12345E-3">>, [{return, tuple}])), % upper case 'E'
      ?assertEqual({ok, 12.345}, argos:decode(<<"12345.0e-3">>, [{return, tuple}])),
      ?assertEqual({ok, 12.345}, argos:decode(<<"0.12345E2">>, [{return, tuple}])),
      ?assertEqual({ok, 12.345}, argos:decode(<<"0.12345e+2">>, [{return, tuple}])), % exponent part can begin with plus sign
      ?assertEqual({ok, 12.345}, argos:decode(<<"0.12345E+2">>, [{return, tuple}])),
      ?assertEqual({ok, -12.345}, argos:decode(<<"-0.012345e3">>, [{return, tuple}])),
     % float: invalid format",
      ?assertMatch({error, _}, argos:decode(<<".123">>, [{return, tuple}])),  % omitted integer part
      ?assertMatch({error, _}, argos:decode(<<"0.">>, [{return, tuple}])),    % omitted fraction part: EOS
      ?assertMatch({error, _}, argos:decode(<<"0.e+3">>, [{return, tuple}])), % omitted fraction part: with exponent part
      ?assertMatch({error, _}, argos:decode(<<"0.1e">>, [{return, tuple}])),    % imcomplete fraction part
      ?assertMatch({error, _}, argos:decode(<<"0.1e-">>, [{return, tuple}])),   % imcomplete fraction part
      ?assertMatch({error, _}, argos:decode(<<"0.1ee-1">>, [{return, tuple}])), % duplicated 'e'
      ?assertMatch({error, _}, argos:decode(<<"0.1e--1">>, [{return, tuple}])), % duplicated sign
      ?assertMatch({error, _}, argos:decode(<<"0.1.2">>, [{return, tuple}])),   % duplicated '.': interpreted as individual tokens
      ok.

argos_decode_strings_test() ->
     %% Strings
     % simple string
     ?assertEqual({ok, <<"abc def">>}, argos:decode(<<"\"abc def\"">>, [{return, tuple}])),
     ?assertEqual({ok, <<"abc def">>}, argos:decode("\"abc def\"", [{return, tuple}])),
     ?assertEqual({ok, <<" abc def ">>}, argos:decode('" abc def "', [{return, tuple}])),
     % string: escaped characters",
     Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
     Expected = {ok, <<"\"\/\\\b\f\n\r\t">>},
     ?assertEqual(Expected, argos:decode(Input, [{return, tuple}])),
     % string: escaped Unicode characters",
     %% japanese
     Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
     Expected1 = [12354,12356,12358,12360,12362],  %  <<"あいうえお">> UTF-8
     {ok, I1} = argos:decode(Input1, [{return, tuple}]),
     ?assertEqual(Expected1, unicode:characters_to_list(I1)),

     %% ascii
     Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
     Expected2 = {ok, <<"abc">>},
     ?assertEqual(Expected2, argos:decode(Input2, [{return, tuple}])),

     %% other multi-byte characters
     Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
     Expected3 = [1757,1758,4270,4271], % <<"۝۞ႮႯ">>
     {ok, I3} = argos:decode(Input3, [{return, tuple}]),
     ?assertEqual(Expected3, unicode:characters_to_list(I3)),

     %% mixture of ascii and japanese characters
     Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
     Expected4 = [97,12354,49,12356,98,98,12358,50,50,12360,99,99,99,12362,
 51,51,51],  % <<"aあ1いbbう22えcccお333">> UTF-8
     {ok, I4} = argos:decode(Input4, [{return, tuple}]),
     ?assertEqual(Expected4, unicode:characters_to_list(I4)),
     % string: surrogate pairs
     Input5    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
     %Expected5 = "𢁉𢂚𢃼", % TODO handle UTF16
     %?assertEqual(Expected5, argos:decode(Input5, [{return, tuple}])) ,
     ?assertMatch({error, _} , catch argos:decode(Input5, [{return, tuple}])) ,
     ok.

argos_decode_arrays_test() ->
     %% Arrays
     % simple array",
     Input6    = <<"[1,2,\"abc def\",null]">>,
     Expected6 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected6, argos:decode(Input6, [{return, tuple}])),
     % array: contains whitespaces
     Input7    = <<"[  1,\t2, \n \"abc def\",\r null]">>,
     Expected7 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected7, argos:decode(Input7, [{return, tuple}])),
     % empty array
     ?assertEqual({ok, []}, argos:decode(<<"[]">>, [{return, tuple}])),
     ?assertEqual({ok, []}, argos:decode(<<"[ \t\r\n]">>, [{return, tuple}])),
     % array: trailing comma is disallowed
     Input8 = <<"[1, 2, \"abc def\", null, ]">>,
     ?assertMatch({error, _}, argos:decode(Input8, [{return, tuple}])),
     % array: missing comma
     Input9 = <<"[1 2, \"abc def\", null]">>, % a missing comma between '1' and '2'
     ?assertMatch({error, _}, argos:decode(Input9, [{return, tuple}])),
     % array: missing closing bracket
     Input10 = <<"[1, 2, \"abc def\", null">>,
     ?assertMatch({error, _}, argos:decode(Input10, [{return, tuple}])),
     ok.

argos_decode_objects_test() ->
     %% Objects
     % simple object -> Record TODO

     % simple object -> Struct
     Input11   = <<"{\"1\":2,\"key\":\"value\"}">>,
     Expected11 = {ok, [{<<"1">>,2},{<<"key">>,<<"value">>}]},
     ?assertEqual(Expected11, argos:decode(Input11, [{return, tuple}])),
     % empty object
     ?assertEqual({ok, []}, argos:decode(<<"{}">>, [{return, tuple}])),
     % simple object -> map
     % object: trailing comma is disallowed
     Input12 = <<"{\"1\":2, \"key\":\"value\", }">>,
     ?assertMatch({error, _}, argos:decode(Input12, [{return, tuple}])),
     % object: missing comma
     Input13 = <<"{\"1\":2 \"key\":\"value\"}">>,
     ?assertMatch( {error, _}, argos:decode(Input13, [{return, tuple}])),
     % object: missing field key
     Input14 = <<"{:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _} , argos:decode(Input14, [{return, tuple}])),
     % object: non string key
     Input15 = <<"{1:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _} , argos:decode(Input15, [{return, tuple}])),
     % object: missing field value
     Input16 = <<"{\"1\", \"key\":\"value\"}">>,
     ?assertMatch({error, _} , argos:decode(Input16, [{return, tuple}])),
     % object: missing closing brace
     Input17 = <<"{\"1\":2 \"key\":\"value\"">>,
     ?assertMatch({error, _} , argos:decode(Input17, [{return, tuple}])),
     %% BINARY
     Input18 = <<"{\"1\":2,\"key\":\"value\"}">>,
     %% PROPLIST
     % object: proplist / binary k
     ?assertMatch( {ok,[{<<"1">>,2},{<<"key">>,"value"}]}, argos:decode(Input18, [{return, tuple}, {binary, k}, {mode, proplist}])),
     % object: proplist / binary v
     ?assertMatch( {ok,[{'1',2},{key,<<"value">>}]} , argos:decode(Input18, [{return, tuple}, {binary, v}, {mode, proplist}])),
     % object: proplist / binary kv
     ?assertMatch( {ok,[{<<"1">>,2},{<<"key">>,<<"value">>}]} , argos:decode(Input18, [{return, tuple}, {binary, kv}, {mode, proplist}])),
     %% MAP
     % object: map / binary k
     ?assertMatch({ok,#{<<"1">> := 2,<<"key">> := "value"}} , argos:decode(Input18, [{return, tuple}, {binary, k}, {mode, map}])),
     % object: map / binary v
     ?assertMatch({ok,#{'1' := 2,key := <<"value">>}} , argos:decode(Input18, [{return, tuple}, {binary, v}, {mode, map}])),
     % object: map / binary kv
     ?assertMatch({ok,#{<<"1">> := 2,<<"key">> := <<"value">>}} , argos:decode(Input18, [{return, tuple}, {binary, kv}, {mode, map}])),
     %% RECORD
     % object: record / binary k (does not change)
     ?assertMatch({ok,{'132007953',2,"value"}} , argos:decode(Input18, [{return, tuple}, {binary, k}, {mode, record}])),
     % object: record / binary v
     ?assertMatch( {ok,{'83030046',2,<<"value">>}} , argos:decode(Input18, [{return, tuple}, {binary, v}, {mode, record}])),
     % object: record / binary kv
     ?assertMatch({ok,{'83030046',2,<<"value">>}} , argos:decode(Input18, [{return, tuple}, {binary, kv}, {mode, record}])),
     ok.


