defmodule Avz.App do
    use Application
    use Supervisor
    require Logger

    @impl true
    def init([]), do: Supervisor.init([], strategy: :one_for_one)

    @impl true
    def start(_,arg), do: Supervisor.start_link(__MODULE__, arg, name: __MODULE__)

    @impl true
    def stop(_), do: :ok

end
