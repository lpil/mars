defmodule Mars.Instruction do
  @moduledoc """
  Applying instructions to Robots.
  """
  alias Mars.Robot

  @max_x 9
  @max_y 9


  @doc """
  Apply an instruction to robot state, returning a new robot state after the
  instruction have been performed. Does not do bounds checking.

      iex> %Mars.Robot{ x: 0, y: 0, direction: "N" }
      ...> |> Mars.Instruction.perform("L")
      %Mars.Robot{ x: 0, y: 0, direction: "W" }
  """
  def perform(%Robot{} = robot, cmd) when cmd in ["L", "R"] do
    direction = rotate(robot.direction, cmd)
    %{ robot | direction: direction }
  end

  def perform(%Robot{direction: "N", y: y} = robot, "F") do
    %{ robot | y: y + 1 }
  end
  def perform(%Robot{direction: "S", y: y} = robot, "F") do
    %{ robot | y: y - 1 }
  end
  def perform(%Robot{direction: "E", x: x} = robot, "F") do
    %{ robot | x: x + 1 }
  end
  def perform(%Robot{direction: "W", x: x} = robot, "F") do
    %{ robot | x: x - 1 }
  end


  defp rotate("N", "R"), do: "E"
  defp rotate("E", "R"), do: "S"
  defp rotate("S", "R"), do: "W"
  defp rotate("W", "R"), do: "N"

  defp rotate("N", "L"), do: "W"
  defp rotate("W", "L"), do: "S"
  defp rotate("S", "L"), do: "E"
  defp rotate("E", "L"), do: "N"
end
