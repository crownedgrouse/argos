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
%-export([decode_file/1, decode_file/2]).
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
decode(D, O) when is_binary(D), is_list(O)
    -> 
        Opt = argos_lib:options(O),
        decode(D, Opt);

decode(D, Opt) when is_binary(D), is_record(Opt, opt)
    -> 
        json:decode(D).
