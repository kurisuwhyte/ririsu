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


defmodule Ririsu do
  @moduledoc """
  Ririsu --- the 2^6th most horrible language ever devised.

  Usage:
    ririsu help
    ririsu version
    ririsu eval <expression> <files...>
    ririsu repl <files...>
    ririsu <files...>
  """
  alias Ririsu.Interpreter, as: Interpreter
  alias Ririsu.Repl,        as: Repl

  def version, do: "0.4.1"

  def usage do
    {_, help} = Ririsu.__info__(:moduledoc)
    IO.puts help
  end

  def main(["help"]),    do: usage
  def main(["version"]), do: IO.puts "ririsu #{Ririsu.version}"

  def main(["eval", expr]),               do: execute(expr)
  def main(["eval", expr | files]),       do: execute(contents(files) <> expr)

  def main(["repl"]),                     do: Repl.run
  def main(["repl" | files]),             do: Repl.run eval(contents(files))

  def main(files) when length(files) > 0, do: execute(contents(files))

  def main(_) do
    usage
    System.halt 1
  end

  defp eval(source),      do: eval(source, {0, :dict.new, []})
  defp eval(source, env), do: Interpreter.run(String.to_char_list!(source), env)
  
  defp execute(source), do: execute(source, {0, :dict.new, []})
  defp execute(source, env) do
    {_, _, r} = eval(source, env)
    IO.inspect r
  end

  defp readAll(files),  do: Enum.map(files, &File.read!/1)
  defp contents(files) do
    List.foldr readAll(files), "",
               fn (a, b) -> a <> b end
  end

end
