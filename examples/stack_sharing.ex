defmodtype StackT do
  $param item
  $opaque s
  callback new : list(item) -> s
  callback push : item -> s -> s
end

defmodule StackInt do
  @behaviour StackT
  $param item = int
  $type s = list(int)
  def new (l : list(item)) : s = l
  def push(x : item, st : s) : s = List.cons(x, st)
end

# Define a push function generic in the implementation of an integer stack
defmodule Test do
  def push_zero(m : StackT[item = int], st : m.s) : m.s =
    m.push(0, st)
  def test() : StackInt.s = push_zero(StackInt, StackInt.new([1]))
end
