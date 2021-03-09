defmodule Avz.MixProject do
 use Mix.Project

 def project do
    [
      app: :avz,
      version: "6.0.0",
      description: "AVZ Social Auth",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      erlc_paths: ["src"],
      elixirc_options: [warnings_as_errors: true],
      deps: deps()
    ]
 end

 def application do
    [
      mod: {Avz.App, []},
      extra_applications: [:logger],
      applications: [
        :asn1,
        :public_key,
        :ssl,
        :n2o,
        :nitro,
        :oauth
      ]
    ]
 end

 def deps do
  [
    {:n2o,   github: "synrc/n2o",       ref: "master", override: true},
    {:nitro, github: "synrc/nitro",     ref: "master", override: true},
    {:oauth, github: "tim/erlang-oauth",ref: "v2.0.0", compile: "erl -make"}
  ]
 end
end
