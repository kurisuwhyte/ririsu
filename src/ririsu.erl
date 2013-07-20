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
-export([run/1]).

%% =====================================================================
%% Public API
%% =====================================================================

%% 
%% Evaluates a Ririsu program.
%%
%% :: [A] -> [B]
run(Source) -> 
    {0, _, Res} = do_in_environment(Source, {0, dict:new(), []}),
    Res.


%% =====================================================================
%% Internal functions
%% =====================================================================

%%
%% Evaluates a source in a given environment.
%%
do_in_environment(Source, Initial) ->
    lists:foldl(fun(A, B) -> evaluate([A], B) end, Initial, Source).

%%
%% Evaluates a source in a given environment, returns the result.
%%
run_in_environment(Source, Initial) ->
    {0, _, Res} = do_in_environment(Source, Initial),
    Res.

%%
%% Modulo operation
%%
mod(A, B) -> (A rem B + B) rem B.

%% =====================================================================
%% Primitive operations
%% =====================================================================

%%% Duplicate A
%%% :: A, [Dict, [B | C]] -> [Dict, [B B | C]]
evaluate("^", {0, Env, [A|Tail]}) ->
    {0, Env, [A, A|Tail]};

%%% Swap A B
%%% :: A, [Dict, [B C | D]] -> [Dict, [C B | D]]
evaluate("~", {0, Env, [A, B|Tail]}) ->
    {0, Env, [B, A|Tail]};

%%% Drop A
%%% :: A, [Dict, [B | C]] -> [Dict, [C]]
evaluate(" ", {0, Env, [_|Tail]}) ->
    {0, Env, Tail};

%%% Define A B
%%% :: A, [Dict, [B C | D]] -> [Dict{B => C}, D]
evaluate("@", {0, Env, [Name, Code|Tail]}) ->
    F = lists:reverse(lists:flatten(Code)),
    {0, dict:store(Name, F, Env), Tail};

%%% Evaluate A
evaluate("$", {0, Env, [Source|Stack]}) ->
    do_in_environment(Source, {0, Env, Stack});

%%% Plus A B
evaluate("+", {0, Env, [A, B|Stack]}) ->
    {0, Env, [integer_to_list(list_to_integer(A) + list_to_integer(B)) | Stack]};

%%% Minus A B
evaluate("-", {0, Env, [A, B|Stack]}) ->
    {0, Env, [integer_to_list(list_to_integer(A) - list_to_integer(B)) | Stack]};

%%% Divide A B
evaluate("/", {0, Env, [A, B|Stack]}) ->
    {0, Env, [integer_to_list(list_to_integer(A) / list_to_integer(B)) | Stack]};

%%% Multiply A B
evaluate("*", {0, Env, [A, B|Stack]}) ->
    {0, Env, [integer_to_list(list_to_integer(A) * list_to_integer(B)) | Stack]};

%%% Modulo A B
evaluate("%", {0, Env, [A,B|Stack]}) ->
    {0, Env, [integer_to_list(mod(list_to_integer(A), list_to_integer(B))) | Stack]};

%%% Cons A B
evaluate(":", {0, Env, [A, B|Stack]}) ->
    {0, Env, [[A | B] | Stack]};

%%% Concat A B
evaluate("&", {0, Env, [A, B|Stack]}) ->
    {0, Env, [A ++ B | Stack]};

%%% Equal A B
evaluate("=", {0, Env, [A, B|Stack]}) ->
    {0, Env, [A == B | Stack]};

%%% Greater A B
evaluate(">", {0, Env, [A, B|Stack]}) ->
    {0, Env, [A > B | Stack]};

%%% Not A
evaluate("!", {0, Env, [A|Stack]}) ->
    {0, Env, [not(A) | Stack]};

%%% Either A B
evaluate("?", {0, Env, [A, B, C | Stack]}) ->
    if A    -> do_in_environment(B, {0, Env, Stack});
       true -> do_in_environment(C, {0, Env, Stack})
    end;

%%% Map F A
evaluate("|", {0, Env, [F, Xs | Stack]}) ->
    G   = lists:reverse(lists:flatten(F)),
    Res = lists:map(fun(X) -> run_in_environment(G
                                               , {0, Env, [X | Stack]})
                    end
                    , Xs),
    {0, Env, [Res | Stack]};

%%% Filter F A
evaluate("#", {0, Env, [F, Xs | Stack]}) ->
    G   = lists:reverse(lists:flatten(F)),
    Res = lists:filter(fun(X) -> 
                               [A|_] = run_in_environment(G
                                                          , {0, Env, [X | Stack]}),
                               A
                       end
                       , Xs),
    {0, Env, [Res | Stack]};


%%% Reduce F Y X
evaluate("\\", {0, Env, [F, Y, Xs | Stack]}) ->
    G   = lists:reverse(lists:flatten(F)),
    Res = lists:foldr(fun(A,B) ->
                              [Head|_] = run_in_environment(G
                                                            , {0, Env, [A,B|Stack]}),
                              Head
                      end
                      , Y, Xs),
    {0, Env, [Res | Stack]};
        

%%% Quote
evaluate("[", {Mode, Env, Stack}) ->
    if Mode =:= 0 -> {1, Env, [[] | Stack]};
       true       -> [Head|Tail] = Stack,
                     {Mode + 1, Env, [["[" | Head] | Tail]}
    end;

%%% Unquote
evaluate("]", {Mode, Env, Stack}) ->
    if Mode =:= 0 -> erlang:error(unquote_outside_data_mode);
       Mode =:= 1 -> {Mode - 1, Env, Stack};
       true       -> [Head|Tail] = Stack,
                     {Mode - 1, Env, [["]" | Head] | Tail]}
    end;

%%% :: A, [Dict{A => B}, C] -> [Dict, D]
evaluate(Op, {0, Env, Stack}) ->
    case dict:find(Op, Env) of
        {ok, Source} -> do_in_environment(Source, {0, Env, Stack});
        error        -> {0, Env, [Op|Stack]}
    end;

%%% :: A, [Dict, B] -> [Dict, [A | B]]
evaluate(X, {N, Env, [As | Stack]}) when N > 0 ->
    {N, Env, [[X | As] | Stack]}.