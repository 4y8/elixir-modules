defmodtype GenType do
  $param request
  $param response
  $opaque state

  callback handle_call : request -> state -> response
end

defmodule Stack do
  $param a

  @behaviour GenType
  $param request = { :push, a }
  $param response = a
  $type state = list(a)
end