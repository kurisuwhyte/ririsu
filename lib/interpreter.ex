# Copyright (c) 2013 Christina Whyte (kurisu.whyte@gmail.com)
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


defmodule Ririsu.Interpreter do
  @moduledoc """
  The evaluator for the Ririsu language.
  """
  
  @doc """
  Runs a Ririsu source code.

  The runner takes a character list, and an initial state, and it
  returns a new  state. The state is a Tuple in the form:

      { Mode :: Number, Environment :: Dict, Stack :: List }

  `run/1` will call `run/2` with an empty initial state.
  """
  def run(source) do
    run(source, { 0, :dict.new, [] })
  end

  def run(source, initial) do
    List.foldl(source, initial, &op/2)
  end


  # -- Some private helpers --------------------------------------------
  defp mod(a, b) do
    rem(rem(a, b + b), b)
  end

  defp unfold(a, f) do
    unfold([], a, f)
  end

  defp unfold(xs, a, f) do
    case f.(a) do
      :stop    -> xs
      {:ok, x} -> [x | xs] ++ unfold(xs, x, f)
    end
  end


  # -- Evaluates the Ririsu primitive operations -----------------------

  # Newlines are ignored
  def op(?\n, {mode, env, stack}) do
    {mode, env, stack}
  end

  # Enters quoting mode
  def op(?[, {mode, env, stack}) do
    if mode == 0 do
      {1, env, [ [] | stack ]}
    else
      [head | tail] = stack
      { mode + 1, env, [ [?[ | head] | tail]}
    end
  end

  # Exits quoting mode
  def op(?], {mode, env, stack}) do
    cond do
      mode == 0 -> :erlang.error(:unquote_outside_data_mode)
      mode == 1 ->
        [head | tail] = stack
        {mode - 1, env, [:lists.reverse(head) | tail]}
      true ->
        [head | tail] = stack
        {mode - 1, env, [ [?] | head] | tail ]}
    end
  end

  # -- Basic combinators ---------------------------------------------
  @doc "duplicate (↠)           [A _] → [A A _]"
  def op(?↠, {0, env, [a | stack]}) do
    {0, env, [a, a | stack]}
  end

  @doc "swap (⇄)                [A B _] → [B A _]"
  def op(?⇄, {0, env, [a, b | stack]}) do
    {0, env, [b, a | stack]}
  end

  @doc "drop (↓)               [A _] → [_]"
  def op(?↓, {0, env, [_ | stack]}) do
    {0, env, stack}
  end

  @doc "concatenate (⊕)         [[A] [B] _] → [[A B] _]"
  def op(?⊕, {0, env, [a, b | stack]}) do
    {0, env, [List.concat(a, b) | stack]}
  end

  @doc "cons (×)                [A [B] _] → [[A B] _]"
  def op(?×, {0, env, [a, b | stack]}) do
    {0, env, [[a | b] | stack]}
  end

  @doc "unit (∘)                [A _] → [[A] _]"
  def op(?∘, {0, env, [a | stack]}) do
    {0, env, [[a] | stack]}
  end

  @doc "i (▶)                   [[A] _] → [A _]"
  def op(?▶, {0, env, [a | stack]}) do
    run(a, {0, env, stack})
  end

  @doc "dip (↝)                 [[A] B _] → [B A _]"
  def op(?↝, {0, env, [a, b | stack]}) do
    {_, _, [c | _]} = run(a, {0, env, stack})
    {0, env, [b, c | stack]}
  end


  # -- Bindings and dynamic evaluation ---------------------------------
  @doc "define (@)              [A B _] → [_]"
  def op(?@, {0, env, [name, code | stack]}) do
    {0, :dict.store(name, code, env), stack}
  end


  # -- Arithmetic ------------------------------------------------------
  @doc "addition (+)            [A B _] → [C _]"
  def op(?+, {0, env, [a, b | stack]}) do
    {0, env, [a + b | stack]}
  end

  @doc "subtraction (-)         [A B _] → [C _]"
  def op(?-, {0, env, [a, b | stack]}) do
    {0, env, [a - b | stack]}
  end

  @doc "division (/)            [A B _] → [C _]"
  def op(?/, {0, env, [a, b | stack]}) do
    {0, env, [a / b | stack]}
  end

  @doc "multiplication (*)      [A B _] → [C _]"
  def op(?*, {0, env, [a, b | stack]}) do
    {0, env, [a * b | stack]}
  end

  @doc "modulo (%)             [A B _] → [C _]"
  def op(?%, {0, env, [a, b | stack]}) do
    {0, env, [mod(a, b) | stack]}
  end

  @doc "square-root (√)         [A _] → [B _]"
  def op(?√, {0, env, [a | stack]}) do
    {0, env, [:math.sqrt(a) | stack]}
  end

  @doc "round (⎨)               [A _] → [B _]"
  def op(?⎨, {0, env, [a | stack]}) do
    {0, env, [:erlang.round(a) | stack]}
  end


  # -- Conversions -----------------------------------------------------

  @doc "list→integer (i)        [[A] _] → [B _]"
  def op(?i, {0, env, [a | stack]}) do
    {0, env, [list_to_integer(a) | stack]}
  end

  @doc "integer→list (a)        [A _] → [[B] _]"
  def op(?a, {0, env, [a | stack]}) do
    {0, env, [integer_to_list(a) | stack]}
  end


  # -- Logic -----------------------------------------------------------

  @doc "equals (=)              [A B _] → [bool _]"
  def op(?=, {0, env, [a, b | stack]}) do
    {0, env, [a == b | stack]}
  end

  @doc "greater-than (>)        [A B _] → [bool _]"
  def op(?>, {0, env, [a, b | stack]}) do
    {0, env, [a > b | stack]}
  end

  @doc "negate (¬)              [A _] → [bool _]"
  def op(?¬, {0, env, [a | stack]}) do
    {0, env, [!a | stack]}
  end

  @doc "bottom (⊥)              [_] → [bool _]"
  def op(?⊥, {0, env, stack}) do
    {0, env, [false | stack]}
  end

  @doc "branch (⌥)              [[A] [B=>C] [D=>E] _] → [(C | E) _]"
  def op(?⌥, {0, env, [test, consequent, alternate | stack]}) do
    if test, do:   run(consequent, {0, env, stack}),
             else: run(alternate, {0, env, stack})
  end


  # -- Lists -----------------------------------------------------------

  @doc "reverse (≪)             [[A] _] → [[A] _]"
  def op(?≪, {0, env, [xs | stack]}) do
    {0, env, [:lists.reverse(xs) | stack]}
  end

  @doc "flatten (⇲)             [[[A]] _] → [[A] _]"
  def op(?⇲, {0, env, [xs | stack]}) do
    {0, env, [List.flatten(xs) | stack]}
  end

  @doc "head (⊤)                [[A B] _] → [A B _]"
  def op(?⊤, {0, env, [[head | tail] | stack]}) do
    {0, env, [head, tail | stack]}
  end


  # -- Folds -----------------------------------------------------------

  @doc "map (→)                 [[A=>B] [A] _] → [[B] _]"
  def op(?→, {0, env, [f, xs | stack]}) do
    r = Enum.map(xs,
                 fn (x) -> {_,_,[h | _]} = run(f, {0, env, [x | stack]})
                           h
                 end)
    {0, env, [r | stack]}
  end

  @doc "filter (⋈)              [[A=>bool] [A] _] → [[A] _]"
  def op(?⋈, {0, env, [f, xs | stack]}) do
    r = Enum.filter(xs,
                    fn (x) -> {_,_,[h | _]} = run(f, {0, env, [x | stack]})
                              h
                    end)
    {0, env, [r | stack]}
  end

  @doc "fold (⚺)                [[A B=>C] B [A] _] → [[C] _]"
  def op(?⚺, {0, env, [f, initial, xs | stack]}) do
    r = List.foldr(xs, initial,
                   fn (a, b) -> {_,_,[h | _]} = run(f, {0, env, [a, b | stack]})
                                h
                   end)
    {0, env, [r | stack]}
  end

  @doc "unfold (⚻)              [[A=>maybe B] A _] → [[B] _]"
  def op(?⚻, {0, env, [f, initial | stack]}) do
    r = unfold(initial,
               fn (x) -> {_,_,[h | _]} = run(f, {0, env, [x | stack]})
                         if h == false, do:   :stop,
                                        else: {:ok, h}

               end)
    {0, env, [r | stack]}
  end

  # -- Base cases ------------------------------------------------------
  @doc "maybe-run (_)           [_] → [A _]"
  def op(x, {0, env, stack}) do
    case :dict.find(x, env) do
      {:ok, source} -> run(source, {0, env, stack})
      :error        -> {0, env, [x | stack]}
    end
  end

  @doc "id (_)                  [_] → [A _]"
  def op(x, {n, env, [as | stack]}) when n > 0 do
    {n, env, [[x | as] | stack]}
  end
end
