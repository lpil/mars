defmodule Mars.WorldTest do
  use ExUnit.Case

  alias  Mars.Robot
  alias  Mars.World
  import Mars.World

  # check/2

  test "outside of north bounds" do
    world = %World{ max_x: 3, max_y: 8 }
    robot = %Robot{ x: 0, y: 9 }
    assert check(world, robot) == :lost
  end

  test "outside of east bounds" do
    world = %World{ max_x: 3, max_y: 8 }
    robot = %Robot{ x: 4, y: 8 }
    assert check(world, robot) == :lost
  end

  test "outside of south bounds" do
    world = %World{ max_x: 11, max_y: 3 }
    robot = %Robot{ x: 4, y: -1 }
    assert check(world, robot) == :lost
  end

  test "outside of west bounds" do
    world = %World{ max_x: 1, max_y: 3 }
    robot = %Robot{ x: -1, y: 0 }
    assert check(world, robot) == :lost
  end

  test "outside of south east bounds" do
    world = %World{ max_x: 11, max_y: 3 }
    robot = %Robot{ x: 14, y: -1 }
    assert check(world, robot) == :lost
  end

  # Scent adding and checking

  test "scents can be put and checked" do
    world = %World{}
    refute world |> scent?(%{x: 0, y: 0})

    world = put_scent(world, %{x: 0, y: 0})
    assert world |> scent?(%{x: 0, y: 0})

    world = put_scent(world, %{x: 10, y: 500})
    assert world |> scent?(%{x: 0, y: 0})
    assert world |> scent?(%{x: 10, y: 500})
  end
end
