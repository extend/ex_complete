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

%% @type binding() = {string(), integer() | undefined}.

-module(ex_complete).
-author('Anthony Ramine <nox@dev-extend.eu>').
-behaviour(gen_server).

-export([start_link/0,
         bindings/0,
         expand/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {bindings = [], shell}).


%% @spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {}, [{timeout, infinity}]).

%% @spec bindings() -> [binding()]
bindings() ->
  gen_server:call(?MODULE, bindings, infinity).

%% @spec expand(string()) -> {yes | no, string(), [{string(), term()}]}
expand(Input) ->
  gen_server:call(?MODULE, {expand, Input}, infinity).

%% @hidden
init({}) ->
  User = whereis(user),
  [{user_drv, UserDrv}] = group:interfaces(User),
  [{current_group, Group}] = user_drv:interfaces(UserDrv),
  {shell, Shell} = lists:keyfind(shell, 1, group:interfaces(Group)),
  erlang:trace_pattern({shell, shell_rep, 4},
                       [{'_', [], [{return_trace}]}], [local]),
  erlang:trace(Shell, true, [call]),
  io:setopts(Group, [{expand_fun, fun (I) -> ex_complete:expand(I) end}]),
  {ok, #state{shell = Shell}}.

%% @hidden
handle_call(bindings, _From, S = #state{bindings = Bs}) ->
  {reply, Bs, S};
handle_call({expand, Input}, _From, S = #state{bindings = Bs}) ->
  case Input of
    "" ->
      {no, "", Ms} = edlin_expand:expand(Input),
      {reply, {no, "", Bs ++ Ms}, S};
    I ->
      case edlin_expand:expand(I) of
        C = {yes, _PT, []} -> {reply, C, S};
        C = {no, "", Ms} when Ms =/= [] -> {reply, C, S};
        _ -> {reply, expand(Input, Bs), S} end end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info({trace, Shell, return_from, {shell, shell_rep, 4}, {_, _, Bs, _}},
            S = #state{shell = Shell}) ->
  NewBs = [ {atom_to_list(N), arity(V)} || {N, V} <- Bs ],
  {noreply, S#state{bindings = NewBs}};
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @spec expand(string(), [binding()]) -> {yes | no, string(), [binding()]}
expand(Input, Bs) ->
  {_, Word, _} = edlin:over_word(Input, [], 0),
  Matches = matches(Word, Bs),
  case Matches of
    [{Word, undefined}] -> {yes, "", []};
    [{Word, _A}] -> {yes, "", Matches};
    [{N, _A}] -> {yes, lists:nthtail(length(Word), N), []};
    [] -> {no, "", []};   
    Ms ->
      P = longest_common_prefix([ N || {N, _A} <- Ms ]),
      case lists:nthtail(length(Word), P) of
        "" -> {no, "", Ms};
        PT -> {yes, PT, []} end end.

%% @spec matches(string(), [binding()]) -> [binding()]
matches(Word, Bs) ->
  matches(Word, Bs, []).

%% @hidden
matches(Word, [B = {N, _A} | Bs], Acc) ->
  case lists:prefix(Word, N) of
    true -> matches(Word, Bs, [B | Acc]);
    false -> matches(Word, Bs, Acc) end;
matches(_Word, [], Acc) ->
  Acc.

%% @spec longest_common_prefix([list()]) -> list()
longest_common_prefix([L | Tail]) ->
  longest_common_prefix1(Tail, L).

%% @hidden
longest_common_prefix1(_Ls, "") ->
  "";
longest_common_prefix1([L | Tail], Acc) ->
  longest_common_prefix1(Tail, longest_common_prefix(L, Acc));
longest_common_prefix1([], Acc) ->
  Acc.

%% @spec longest_common_prefix(list(), list()) -> list()
longest_common_prefix(L1, L2) ->
  longest_common_prefix2(L1, L2, []).

%% @hidden
longest_common_prefix2([E | L1], [E | L2], Acc) ->
  longest_common_prefix2(L1, L2, [E | Acc]);
longest_common_prefix2(_, _, Acc) ->
  lists:reverse(Acc).

%% @spec arity(term()) -> integer() | undefined
arity(F) when is_function(F) ->
  {arity, A} = erlang:fun_info(F, arity),
  A;
arity(_) ->
  undefined.
