defmodule DataStoreCompat do
	@moduledoc """
	Behaviour defining a simple key/value store API where state is
	threaded through function calls.
	"""

	@type key :: atom() | integer() | String.t()
	@type value :: term()
	@type error :: term()
  # for state we use @type instead of @opaque to avoid warnings about unused types in the behaviour
	@type state :: term()

	@callback start_link(keyword()) :: {:ok, state()} | {:error, error()}
	@callback put(state(), key(), value()) :: {:ok, state()} | {:error, error()}
	@callback get(state(), key()) :: {{:ok, value()} | :not_found, state()}
	@callback delete(state(), key()) :: {:ok, state()} | {:error, error()}
end

defmodule StoreProviderCompat do
	@moduledoc """
	Behaviour showing that modules are first-class values: callbacks can
	accept store modules as inputs and return store modules as results.

	Note: in current Elixir typespecs, dependent module-state typing such as
	`X : DataStore, x : X.state` cannot be expressed directly, so this uses
	`module()` and `term()` where needed.
	"""

	@type local_data_store :: module()

	@callback default_store() :: local_data_store()
	@callback normalize_store(local_data_store()) :: local_data_store()
	@callback put_via(local_data_store(), term(), DataStoreCompat.key(), DataStoreCompat.value()) ::
		{:ok, term()} | {:error, DataStoreCompat.error()}
end

defmodule MemoryStoreCompat do
	@moduledoc """
	In-memory implementation of `DataStoreCompat` using an immutable map state.
	"""

	@behaviour DataStoreCompat

	@opaque state :: %{DataStoreCompat.key() => DataStoreCompat.value()}

	@impl DataStoreCompat
	def start_link(opts \\ []) do
		initial = Keyword.get(opts, :initial, %{})
		{:ok, initial}
	end

	@impl DataStoreCompat
	def put(state, key, value) do
		{:ok, Map.put(state, key, value)}
	end

	@impl DataStoreCompat
	def get(state, key) do
		case Map.fetch(state, key) do
			{:ok, value} -> {{:ok, value}, state}
			:error -> {:not_found, state}
		end
	end

	@impl DataStoreCompat
	def delete(state, key) do
		{:ok, Map.delete(state, key)}
	end
end

defmodule StaticStoreProviderCompat do
	@moduledoc """
	Simple provider that passes `MemoryStoreCompat` around as a first-class module.
	"""

	@behaviour StoreProviderCompat

	@impl StoreProviderCompat
	def default_store do
		MemoryStoreCompat
	end

	@impl StoreProviderCompat
	def normalize_store(store) do
		store
	end

	@impl StoreProviderCompat
	def put_via(store, state, key, value) do
		store.put(state, key, value)
	end
end

defmodule DemoCompat do
	def run do
		store0 = StaticStoreProviderCompat.default_store()
		store = StaticStoreProviderCompat.normalize_store(store0)

		{:ok, state0} = store.start_link(initial: %{user_id: 7})
		{{:ok, 7}, state1} = store.get(state0, :user_id)

		{:ok, state2} = StaticStoreProviderCompat.put_via(store, state1, :user_id, 42)
		{{:ok, 42}, state3} = store.get(state2, :user_id)

		{:ok, state4} = store.delete(state3, :user_id)
		{:not_found, _state5} = store.get(state4, :user_id)
	end
end

DemoCompat.run()
