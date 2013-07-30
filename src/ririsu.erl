%% -*- erlang-indent-level:4; indent-tabs-mode:nil; fill-column:72   -*-
%% ---------------------------------------------------------------------
%% 
%% Copyright (c) 2013 Christina Whyte (kurisu.whyte@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%% ---------------------------------------------------------------------

-module(ririsu).
-export([run/1, run/2, do/2]).

%% =====================================================================
%% Public API
%% =====================================================================

%% 
%% Evaluates a Ririsu program.
%%
%% :: [A] -> [B]
run(Source) ->
    run(Source, {0, dict:new(), []}).

run(Source, Initial) -> 
    {0, _, Res} = do(Source, Initial),
    Res.

%%
%% Evaluates a source in a given environment.
%%
do(Source, Initial) ->
    lists:foldl(fun evaluate/2, Initial, Source).



%% =====================================================================
%% Internal functions
%% =====================================================================

%%
%% Modulo operation
%%
mod(A, B) -> (A rem B + B) rem B.

%%
%% Constructs a list by applying F to the previous result.
%%
unfold(Fun, A) ->
    unfold(Fun, A, []).

unfold(Fun, A, Xs) ->
    case Fun(A) of
        stop    -> Xs;
        {ok, X} -> [X | Xs] ++ unfold(Fun, X, Xs)
    end.


%% =====================================================================
%% Primitive operations
%% =====================================================================

evaluate($\n, {Mode, Env, Stack}) -> {Mode, Env, Stack};


%% == Quoting / Unquoting ==============================================
evaluate($[, {Mode, Env, Stack}) ->
    if Mode =:= 0 -> {1, Env, [[] | Stack]};
       true       -> [Head | Tail] = Stack,
                     {Mode + 1, Env, [[$[ | Head ] | Tail]}
    end;

evaluate($], {Mode, Env, Stack}) ->
    if Mode =:= 0 -> erlang:error(unquote_outside_data_mode);
       Mode =:= 1 -> [Head | Tail] = Stack,
                     {Mode - 1, Env, [lists:reverse(Head) | Tail]};
       true       -> [Head | Tail] = Stack,
                     {Mode - 1, Env, [[$] | Head] | Tail]}
    end;

evaluate($q, {0, Env, [A | Stack]}) ->
    {0, Env, ["[" ++ A ++ "]" | Stack]};


%% == Stack manipulation ===============================================
evaluate($^, {0, Env, [A | Stack]}) ->
    {0, Env, [A, A | Stack]};

evaluate($~, {0, Env, [A, B | Stack]}) ->
    {0, Env, [B, A | Stack]};

evaluate(32, {0, Env, [_ | Stack]}) ->
    {0, Env, Stack};


%% == Bindings and dynamic evaluation ==================================
evaluate($@, {0, Env, [Name, Code | Stack]}) ->
    {0, dict:store(Name, Code, Env), Stack};

evaluate($$, {0, Env, [Source | Stack]}) ->
    do(Source, {0, Env, Stack});


%% == Arithmetic =======================================================
evaluate($+, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A + B | Stack]};

evaluate($-, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A - B | Stack]};

evaluate($/, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A / B | Stack]};

evaluate($*, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A * B | Stack]};

evaluate($%, {0, Env, [A, B | Stack]}) ->
    {0, Env, [mod(A, B) | Stack]};

evaluate($s, {0, Env, [A | Stack]}) ->
    {0, Env, [math:sqrt(A) | Stack]};

evaluate($o, {0, Env, [A | Stack]}) ->
    {0, Env, [erlang:round(A) | Stack]};


%% == Conversions ======================================================
evaluate($i, {0, Env, [A | Stack]}) ->
    {0, Env, [list_to_integer(A) | Stack]};

evaluate($a, {0, Env, [A | Stack]}) ->
    {0, Env, [integer_to_list(A) | Stack]};


%% == Logic ============================================================
evaluate($=, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A == B | Stack]};

evaluate($>, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A > B | Stack]};

evaluate($!, {0, Env, [A | Stack]}) ->
    {0, Env, [not(A) | Stack]};

evaluate($f, {0, Env, Stack}) ->
    {0, Env, [false | Stack]};

evaluate($?, {0, Env, [Test, Consequent, Alternate | Stack]}) ->
    if Test -> do(Consequent, {0, Env, Stack});
       true -> do(Alternate,  {0, Env, Stack})
    end;

%% == Lists ============================================================
evaluate($:, {0, Env, [A, B | Stack]}) ->
    {0, Env, [[A | B] | Stack]};

evaluate($&, {0, Env, [A, B | Stack]}) ->
    {0, Env, [A ++ B | Stack]};

evaluate($r, {0, Env, [Xs | Stack]}) ->
    {0, Env, [lists:reverse(Xs) | Stack]};

evaluate($v, {0, Env, [Xs | Stack]}) ->
    {0, Env, [lists:flatten(Xs) | Stack]};

evaluate($h, {0, Env, [[Head | Tail] | Stack]}) ->
    {0, Env, [Head, Tail | Stack]};

evaluate($l, {0, Env, [Head | Stack]}) ->
    {0, Env, [[Head] | Stack]};


%% == Folds ============================================================
evaluate($|, {0, Env, [F, Xs | Stack]}) ->
    Res = lists:map(fun(X) -> [H | _] = run(F, {0, Env, [X | Stack]}),
                              H
                    end, Xs),
    {0, Env, [Res | Stack]};

evaluate($#, {0, Env, [F, Xs | Stack]}) ->
    Res = lists:filter(fun(X) -> [H | _] = run(F, {0, Env, [X | Stack]}),
                                 H
                       end, Xs),
    {0, Env, [Res | Stack]};

evaluate($., {0, Env, [F, Initial, Xs | Stack]}) ->
    Res = lists:foldr(fun(A, B) -> [H | _] = run(F, {0, Env, [A, B | Stack]}),
                                   H
                      end, Initial, Xs),
    {0, Env, [Res | Stack]};

evaluate($,, {0, Env, [F, Initial | Stack]}) ->
    Res = unfold(fun(X) ->
                         [H | _] = run(F, {0, Env, [X | Stack]}),
                         if H == false -> stop;
                            true       -> {ok, H}
                         end
                 end, Initial),
    {0, Env, [Res | Stack]};


%% == Base cases =======================================================
evaluate(Op, {0, Env, Stack}) ->
    case dict:find(Op, Env) of
        {ok, Source} -> do(Source, {0, Env, Stack});
        error        -> {0, Env, [Op | Stack]}
    end;

evaluate(X, {N, Env, [As | Stack]}) when N > 0 ->
    {N, Env, [[X | As] | Stack]}.
