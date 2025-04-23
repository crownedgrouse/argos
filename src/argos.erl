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
        try 
            To = Opt#opt.to,
            case argos_lib:valid_to_file(To) of
                skip        -> ok ;
                true        -> put(argos_to, To) ;
                false       -> throw({error, "Invalid 'to' record definition dump file : cannot create"});
                notempty    -> throw({error, "Invalid 'to' record definition dump file : not empty"})
            end,
            Res =
            case Opt#opt.mode of
                otp
                  -> 
                    json:decode(D);
                _ ->
                    Mode = argos_lib:get_decoders(Opt),
                    json:decode(D, [], Mode)
            end,
            throw(Res)
        catch
        throw:Result ->
            case Opt#opt.return of
                    tuple -> {ok, Result};
                    stack -> {ok, Result};
                    _     -> Result
            end;
        error:Reason:Stack -> 
            case Opt#opt.return of
                    tuple -> 
                        {error, Reason};
                    stack -> 
                        {error, Reason, Stack};
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

