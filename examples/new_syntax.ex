defmodule DataStore do
	@moduledoc """
	BEHAVIOUR defining a simple key/value store API where state is
	threaded through function calls.
	Parameters are key, value, and error types.
	The key type is bounded, so that it can be used in a map.
	"""

	$param key: atom() | integer() | String.t()
	$param value
	$param error
	$opaque state

	$callback start_link :: keyword() -> {:ok, state} | {:error, error}
	$callback put :: (state, key, value) -> {:ok, state} | {:error, error}
	$callback get :: (state, key) -> {{:ok, value} | :not_found, state}
	$callback delete :: (state, key) -> {:ok, state} | {:error, error}
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
	$param error
	# let use a transparent type as an alias
	$type localDataStore = DataStore[key=key, value=value, error=error]

	$callback default_store :: () -> localDataStore
	$callback normalize_store :: localDataStore -> localDataStore
	$callback put_via ::
		(X : localDataStore, x : X.state, key, value)
		-> {:ok, X.state} | {:error, error}
end

defmodule MemoryStore do
	@moduledoc """
	MODULE: In-memory implementation of `DataStore` using an immutable map state.
	"""
	$param key: atom() | integer() | String.t()
	$param value
	$param error

	$behaviour DataStore[key=key, value=value, error=error]

	$opaque state = %{key => value}

	@impl DataStore
	def start_link(opts \\ []) do
		initial = Keyword.get(opts, :initial, %{})
		{:ok, initial}
	end

	@impl DataStore
	def put(state, key, value) do
		{:ok, Map.put(state, key, value)}
	end

	@impl DataStore
	def get(state, key) do
		case Map.fetch(state, key) do
			{:ok, value} -> {{:ok, value}, state}
			:error -> {:not_found, state}
		end
	end

	@impl DataStore
	def delete(state, key) do
		{:ok, Map.delete(state, key)}
	end
end

defmodule StaticStoreProvider do
	@moduledoc """
	MODULE: Simple provider that passes `MemoryStore` around as a first-class module.
	"""

	$param key: atom() | integer() | String.t()
	$param value
	$param error

	$behaviour StoreProvider[key=key, value=value, error=error]

	@impl StoreProvider
	def default_store do
		# note here the new syntax for parameterized modules
		MemoryStore[key=key, value=value, error=error]
	end

	@impl StoreProvider
	def normalize_store(store) do
		store
	end

	@impl StoreProvider
	def put_via(store, state, key, value) do
		store.put(state, key, value)
	end
end

defmodule Demo do
	def run do
		# note here the new syntax for parameterized modules
		# note also that we instantite error type to none meaning that do not allow
		# the callbacks to return errors. This is a way to specialize the behaviour
		# for a specific use case.
		alias MyStaticStoreProvider = StaticStoreProvider[key=atom(), value=integer(), error=none()]
		store0 = MyStaticStoreProvider.default_store()
		store = MyStaticStoreProvider.normalize_store(store0)

		{:ok, state0} = store.start_link(initial: %{user_id: 7})
		{{:ok, 7}, state1} = store.get(state0, :user_id)

		{:ok, state2} = MyStaticStoreProvider.put_via(store, state1, :user_id, 42)
		{{:ok, 42}, state3} = store.get(state2, :user_id)

		{:ok, state4} = store.delete(state3, :user_id)
		{:not_found, _state5} = store.get(state4, :user_id)
	end
end

Demo.run()
