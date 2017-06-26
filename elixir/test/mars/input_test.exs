defmodule Mars.InputTest do
  use ExUnit.Case

  alias Mars.Input

  sample_input = """
  5 3
  1 1 E
  RFRFRFRF
  3 2 N
  FRRFLLFFRRFLL
  0 3 W
  LLFFFLFLFL
  """

  @terms sample_input |> Input.parse

  test "max_x is parsed" do
    assert @terms.max_x == 5
  end

  test "max_y is parsed" do
    assert @terms.max_y == 3
  end

  test "instruction_sets are parsed" do
    assert @terms.instruction_sets == [
      %{
        start: %{x: 1, y: 1, direction: "E"},
        instructions: ~w(R F R F R F R F),
      },
      %{
        start: %{x: 3, y: 2, direction: "N"},
        instructions: ~w(F R R F L L F F R R F L L),
      },
      %{
        start: %{x: 0, y: 3, direction: "W"},
        instructions: ~w(L L F F F L F L F L),
      },
    ]
  end
end
