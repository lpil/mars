defmodule Mars.WorldTest do
  use ExUnit.Case

  alias  Mars.Robot
  alias  Mars.World
  import Mars.World, only: [check: 2]

  # Out of bounds

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
end
