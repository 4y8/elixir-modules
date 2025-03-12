defmodtype GenericT do
  $param init
  $param reply
  $param message
  $opaque state

  callback init_state : init -> {:ok, state}
  callback handle_call : message -> state -> {:reply, reply, state}
  callback print_state : state -> reply
end
