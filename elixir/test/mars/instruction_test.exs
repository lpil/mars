defmodule Mars.InstructionTest do
  use ExUnit.Case
  doctest Mars.Instruction

  alias Mars.Robot
  import Mars.Instruction, only: [perform: 2]

  # Rotation

  test "L instruction" do
    robot = %Robot{ x: 0, y: 0, direction: "N" } |> perform("L")
    assert robot == %Robot{ x: 0, y: 0, direction: "W" }
  end

  test "R instruction" do
    robot = %Robot{ x: 0, y: 0, direction: "E" } |> perform("R")
    assert robot == %Robot{ x: 0, y: 0, direction: "S" }
  end

  # Forwards

  test "F instruction going North" do
    robot = %Robot{ x: 0, y: 4, direction: "N" } |> perform("F")
    assert robot == %Robot{ x: 0, y: 5, direction: "N" }
  end

  test "F instruction going East" do
    robot = %Robot{ x: 0, y: 4, direction: "E" } |> perform("F")
    assert robot == %Robot{ x: 1, y: 4, direction: "E" }
  end

  test "F instruction going South" do
    robot = %Robot{ x: 8, y: 4, direction: "S" } |> perform("F")
    assert robot == %Robot{ x: 8, y: 3, direction: "S" }
  end

  test "F instruction going West" do
    robot = %Robot{ x: 6, y: 4, direction: "W" } |> perform("F")
    assert robot == %Robot{ x: 5, y: 4, direction: "W" }
  end
end
