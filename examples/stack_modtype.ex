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

defmodule StackGeneric do
  $param a
  @behaviour StackT
  $param item = a
  $type s = list(item)
  def new (l : list(item)) : s = l
  def push(x : item, st : s) : s = List.cons(x, st)
end

defmodule Test do
  defp x() : _ = StackGeneric[a = int].push(0, StackGeneric[a = int].new([1, 2]))
end