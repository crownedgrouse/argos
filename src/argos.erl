%%%-----------------------------------------------------------------------------
%%% File:      argos.erl
%%% @author    Eric Pailleau <argos@crownedgrouse.com>
%%% @copyright 2025 crownedgrouse.com
%%% @doc
%%% JSON and the Argonauts
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2025-04-12
%%%-----------------------------------------------------------------------------

-module(argos).

-include("argos.hrl").

%-export([encode/1, encode/2]).
-export([decode/1, decode/2]).
-export([decode_file/1, decode_file/2]).
-export([graphql/2, graphql/3]).
%-export([encode_file/2, encode_file/3]).
%-export([pp/1, pp/2, types/0]).
%-export([dump/1, dump/2]).
%-export([decode_stream/1, decode_stream/2]).

%%==============================================================================
%% ----------------------------------
%% @doc decode/1
%% 
%% @end
%% ----------------------------------
decode(D) when is_atom(D)
    -> 
        decode(erlang:atom_to_binary(D), []);

decode(D) when is_binary(D)
    -> 
        decode(D, []);

decode(D) when is_list(D)
    -> 
        decode(erlang:list_to_binary(D)).

%% ----------------------------------
%% @doc decode/2
%% 
%% @end
%% ----------------------------------

decode(D, O) when is_atom(D) 
    -> 
        decode(erlang:atom_to_binary(D), O);

decode(D, O) when is_list(D), is_list(O)
    -> 
        decode(erlang:list_to_binary(D), O);

decode(D, O) when is_binary(D), is_list(O)
    -> 
        Opt = argos_lib:options(O),
        decode(D, Opt);

decode(D, Opt) when is_binary(D), is_record(Opt, opt)
    -> 
        Start = os:timestamp(),
        try 
            To = Opt#opt.to,
            case argos_lib:valid_to_file(To) of
                skip        -> ok ;
                true        -> put(argos_to, To) ;
                {error, R}  -> error(R)
            end,
            {Res, _, B} =
            case Opt#opt.mode of
                otp
                  -> 
                    json:decode(D, [], #{});
                _ ->
                    Mode = argos_lib:get_decoders(Opt),
                    json:decode(D, [], Mode)
            end,
            throw({Res, B})
        catch
        throw:{Result, Bin} ->
            case Opt#opt.return of
                    tuple -> {ok, Result, #{leftover=>Bin, duration=>timer:now_diff(os:timestamp(), Start) / 1000}};
                    _     -> Result
            end;
        error:Reason:Stack -> 
            case Opt#opt.return of
                    tuple when is_atom(Reason) -> 
                        {error, Reason, #{stack => Stack}};
                    tuple when is_tuple(Reason) -> 
                        {ErrAtom, ErrBytes} = Reason,
                        {error, ErrAtom, #{bytes => ErrBytes, stack => Stack}};
                    tuple when is_map(Reason) -> 
                        {error, map_get(code,Reason), #{errmsg => map_get(errmsg,Reason), stack => Stack}};
                    _     -> throw(Reason)
            end
        after
          erase(argos_to)
        end.

%%==============================================================================
%% @doc Decode JSON file
%% @end
-spec decode_file(list()) -> any().

decode_file(F) when is_list(F) 
    -> 
        decode_file(F, []).

%%==============================================================================
%% @doc Decode JSON file with options
%% @end

-spec decode_file(list(), tuple() | list()) -> any().

decode_file(F, O) when is_list(F), is_list(O)
    -> 
        Opt = argos_lib:options(O),
        decode_file(F, Opt);

decode_file(F, Opt) when is_list(F),is_record(Opt, opt) ->
    try
        % Use raw reading, and use real error from read_file otherwise
        B = case erl_prim_loader:get_file(F) of
               error -> 
                    {ok, BA} = file:read_file(F), BA;
               {ok, BB, _} -> 
                    BB
          end,
        throw(argos:decode(B, Opt))
    catch
        throw:Result ->
            Result;
        error:Reason:Stack -> 
            case Opt#opt.return of
                tuple -> {error, Reason};
                stack -> 
                        {error, Reason, Stack};
                _     -> throw(Reason)
            end
   end.

%%==============================================================================
%% @doc GraphQL query
%% @end

graphql(Q, J) -> 
    graphql(Q, J, []).

graphql(Q, _J, O) ->
    Opt = argos_lib:options(O),
    try 
        {ok, L, _} = argos_graphql_lexer:string(Q),
        {ok, R}  = argos_graphql_parser:parse(L),
        R
    catch
        throw:Term   ->  Term ;
        error:Reas:Stack -> 
            case Reas of
                {badmatch, {error,{{Line,Pos}, What, Detail}}}
                    ->  case Opt#opt.return of
                            tuple ->
                                Errmsg = rewrite_errmsg(Detail),
                                {error, parse_error, #{errmsg => Errmsg
                                                     , line => Line
                                                     , position => Pos
                                                     , complainant => What
                                                     , stack => Stack}};
                             _ ->   
                                throw(Reas)
                        end ;
                _ ->    case Opt#opt.return of
                            tuple -> 
                                {error, unexpected_error, #{errmsg => Reas, stack => Stack}};
                             _ ->   
                                throw(Reas)
                        end 
            end            
    end.

rewrite_errmsg([Left, []]) 
    -> 
        Left ++ "eof";
rewrite_errmsg(E)
    ->  lists:flatten(E).