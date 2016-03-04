defmodule Mars do
  @moduledoc """
  Exploring planets.
  """
  alias Mars.Input
  alias Mars.Instruction
  alias Mars.Robot
  alias Mars.World

  @doc """
  Take some raw instructions and explore the planet.
  """
  def explore(input) when is_binary(input) do
    parsed = Input.parse(input)
    world  = %World{ max_x: parsed.max_x, max_y: parsed.max_y }
    explore(world, parsed.instruction_sets)
  end


  defp explore(world, sets, acc \\ [])

  defp explore(_world, [], acc) do
    acc
    |> Enum.reverse
    |> Enum.join("\n")
  end
  defp explore(world, [set|sets], acc) do
    robot = %Robot{} |> Map.merge(set.start)
    {new_world, result} = explore_set(world, robot, set.instructions)
    explore(new_world, sets, [result|acc])
  end


  defp explore_set(world, robot, []) do
    result = robot_to_result(robot)
    {world, result}
  end

  defp explore_set(world, robot, [inst|instructions]) do
    new_robot = Instruction.perform(robot, inst)
    status = World.check(world, new_robot)
    cond do
      status == :lost && World.scent?(world, robot) ->
        explore_set(world, robot, instructions)

      status == :lost ->
        scented_world = World.put_scent(world, robot)
        result = robot_to_result(robot)
        {scented_world, "#{result} LOST"}

      true ->
        explore_set(world, new_robot, instructions)
    end
  end

  defp robot_to_result(robot) do
    [robot.x, robot.y, robot.direction] |> Enum.join(" ")
  end
end
