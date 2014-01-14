defmodule Ririsu.Mixfile do
  use Mix.Project

  def project do
    [ app: :ririsu,
      version: "0.4.1",
      elixir: "~> 0.12.0",
      deps: deps,

      escript_app: nil,
      escript_path: "bin/ririsu",
      escript_main_module: Ririsu
    ]
  end

  def application do
    []
  end

  defp deps do
    []
  end
end