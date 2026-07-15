defmodule DataStore do
	@moduledoc """
	BEHAVIOUR defining a simple key/value store API where state is
	threaded through function calls.
	Parameters are key, value, and error types.
	The key type is bounded, so that it can be used in a map.
	"""

	$param key: atom() or integer() or String.t()
	$param value
	$param start_args
	$opaque error
	$opaque state

	$callback start_link(start_args) = {:ok, state} or {:error, error}
	$callback put(state, key, value) = {:ok, state} or {:error, error}
	$callback get(state, key) = {{:ok, value} or :not_found, state}
	$callback delete(state, key) = {:ok, state} or {:error, error}
end


defmodule StoreProvider do
	@moduledoc """
	BEHAVIOUR showing that modules are first-class values: callbacks can
	accept store modules as inputs and return store modules as results.
	It also includes a dependent-style callback where a state argument
	depends on the module argument.
	"""

	$param key: atom() | integer() | String.t()
	$param value
	# let use a transparent type as an alias
	$type localDataStore = DataStore[key: key, value: value]

	$callback default_store() = localDataStore
	$callback normalize_store(localDataStore) = localDataStore
	$callback put_via(x :: localDataStore, x.state, key, value) =
		          {:ok, x.state} | {:error, x.error}
end
