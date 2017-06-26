Mars
====

## Input

- The first line of input is the upper-right coordinates of the rectangular
  world, the lower-left coordinates are assumed to be 0, 0.
- The remaining input consists of a sequence of robot positions and
  instructions (two lines per robot).
- A position consists of two integers specifying the initial coordinates of
  the robot and an orientation (N, S, E, W), all separated by whitespace on
  one line. A robot instruction is a string of the letters “L”, “R”, and “F”
  on one line.


## Output

- For each robot position/instruction in the input, the output should indicate
  the final grid position and orientation of the robot.
- If a robot falls off the edge of the grid the word “LOST” should be printed
  after the position and orientation.
