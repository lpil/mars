defmodule MarsTest do
  use ExUnit.Case

  @sample_input """
  5 3
  1 1 E
  RFRFRFRF
  3 2 N
  FRRFLLFFRRFLL
  0 3 W
  LLFFFLFLFL
  """

  @sample_output """
  1 1 E
  3 3 N LOST
  2 3 S
  """ |> String.strip

  test "our little robots can explore mars" do
    assert Mars.explore(@sample_input) == @sample_output
  end
end
