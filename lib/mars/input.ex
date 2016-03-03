defmodule Mars.Input do
  @moduledoc """
  Converting instruction strings into actionable terms.
  """

  defstruct max_x: 0,
            max_y: 0,
            instruction_sets: []

  def parse(instructions) when is_binary(instructions) do
    [head|sets] = String.split(instructions, "\n")
    [x, y] = parse_first_line(head)
    %__MODULE__{
      max_x: x,
      max_y: y,
      instruction_sets: parse_sets(sets),
    }
  end


  defp parse_first_line(line) do
    line
    |> String.split(" ")
    |> Enum.map(fn e -> e |> Integer.parse |> elem(0) end)
  end

  defp parse_sets(sets) when is_list(sets) do
    sets
    |> Enum.chunk(2)
    |> Enum.map(fn [start, instructions] ->
      %{
        start: parse_start(start),
        instructions: String.split(instructions, "", trim: true)
      }
    end)
  end

  defp parse_start(start) do
    [x, y, d] = String.split(start, " ")
    {x, _} = Integer.parse(x)
    {y, _} = Integer.parse(y)
    %{ x: x, y: y, direction: d }
  end
end
