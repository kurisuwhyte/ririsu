defmodule Ririsu.Mixfile do
  use Mix.Project

  def project do
    [ app: :ririsu,
      version: "0.4.0",
      elixir: "~> 0.10.1",
      deps: deps,

      escript_app: nil,
      escript_path: "bin/ririsu"
      ]
  end

  def application do
    []
  end
  
  defp deps do
    []
  end
end