%% Copyright (c) 2011, Dev:Extend
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ex_complete_app).
-author('Anthony Ramine <nox@dev-extend.eu>').
-behaviour(application).

-export([start/0]).

-export([start/2, stop/1]).


%% @spec start() -> {ok, pid()} | {error, {already_started, atom()}}
start() ->
  application:start(ex_complete).


%% @hidden
start(_StartType, _StartArgs) ->
  ex_complete_sup:start_link().

%% @hidden
stop(_State) ->
    ok.
