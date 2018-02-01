defmodule FrostFighters do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      Plug.Adapters.Cowboy.child_spec(
        :http, FrostFighters.Router, [], [ dispatch: dispatch() ]
      )
    ]

    opts = [strategy: :one_for_one, name: Navis.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp dispatch do
    [
      {:_,
       [
         {"/ws", FrostFighters.SocketHandler, []},
         {:_, Plug.Adapters.Cowboy.Handler, {FrostFighters.Router, []}}
       ]
      }
    ]
  end
end
