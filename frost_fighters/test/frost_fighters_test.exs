defmodule FrostFightersTest do
  use ExUnit.Case
  doctest FrostFighters

  test "greets the world" do
    assert FrostFighters.hello() == :world
  end
end
