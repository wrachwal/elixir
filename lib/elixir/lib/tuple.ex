defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.

  Tuples are ordered collections of elements; tuples can contain elements of any
  type, and a tuple can contain elements of different types. Curly braces can be
  used to create tuples:

      iex> {}
      {}
      iex> {1, :two, "three"}
      {1, :two, "three"}

  Tuples store elements contiguously in memory; this means that accessing a
  tuple element by index (which can be done through the `Kernel.elem/2`
  function) is a constant-time operation:

      iex> tuple = {1, :two, "three"}
      iex> elem(tuple, 0)
      1
      iex> elem(tuple, 2)
      "three"

  Same goes for getting the tuple size (via `Kernel.tuple_size/1`):

      iex> tuple_size({})
      0
      iex> tuple_size({1, 2, 3})
      3

  Tuples being stored contiguously in memory also means that updating a tuple
  (for example replacing an element with `Kernel.put_elem/3`) will make a copy
  of the whole tuple.

  Tuples are not meant to be used as a "collection" type (which is also
  suggested by the absence of an implementation of the `Enumerable` protocol for
  tuples): they're mostly meant to be used as a fixed-size container for
  multiple elements. For example, tuples are often used to have functions return
  "enriched" values: a common pattern is for functions to return `{:ok, value}`
  for successful cases and `{:error, reason}` for unsuccessful cases. For
  example, this is exactly what `File.read/1` does: it returns `{:ok, contents}`
  if reading the given file is successful, or `{:error, reason}` otherwise
  (e.g., `{:error, :enoent}` if the file doesn't exist).

  This module provides functions to work with tuples; some more functions to
  work with tuples can be found in `Kernel` (`Kernel.tuple_size/1`,
  `Kernel.elem/2`, `Kernel.put_elem/3`, and others).

  ## Record-like API

  This module provides `deftuple/2` and `deftuplep/2` macros similar to
  `Record.defrecord/3` and `Record.defrecordp/3` macros. The only
  difference is that `deftuple/2` and `deftuplep/2` do not introduce
  a tag (atom) as the first element in a resulting tuple data.

  The advantage of having alternate API (featured with named tuple
  elements) when comparing to literal syntax of tuples becomes apparent
  when a tuple has more elements and/or its arity or structure changes
  over time in a complex code.

  Defining such "tag-less records" may seem odd, but it has at least one
  notable use case: in ETS table to store tuples of different shapes
  where the key typically consists of a fixed tag (to identify shape)
  and variable part(s) (to differentiate instances). In such heterogenic
  ETS tables there's also a place for use of records when singular
  instances are appropriate (e.g. to hold "global" counters).
  """

  @doc """
  Creates a new tuple.

  Creates a tuple of `size` containing the
  given `data` at every position.

  Inlined by the compiler.

  ## Examples

      iex> Tuple.duplicate(:hello, 3)
      {:hello, :hello, :hello}

  """
  @spec duplicate(term, non_neg_integer) :: tuple
  def duplicate(data, size) do
    :erlang.make_tuple(size, data)
  end

  @doc """
  Inserts an element into a tuple.

  Inserts `value` into `tuple` at the given `index`.
  Raises an `ArgumentError` if `index` is negative or greater than the
  length of `tuple`. Index is zero-based.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:bar, :baz}
      iex> Tuple.insert_at(tuple, 0, :foo)
      {:foo, :bar, :baz}
      iex> Tuple.insert_at(tuple, 2, :bong)
      {:bar, :baz, :bong}

  """
  @spec insert_at(tuple, non_neg_integer, term) :: tuple
  def insert_at(tuple, index, value) do
    :erlang.insert_element(index + 1, tuple, value)
  end

  @doc """
  Inserts an element at the end of a tuple.

  Returns a new tuple with the element appended at the end, and contains
  the elements in `tuple` followed by `value` as the last element.

  Inlined by the compiler.

  ## Examples
      iex> tuple = {:foo, :bar}
      iex> Tuple.append(tuple, :baz)
      {:foo, :bar, :baz}

  """
  @spec append(tuple, term) :: tuple
  def append(tuple, value) do
    :erlang.append_element(tuple, value)
  end

  @doc """
  Removes an element from a tuple.

  Deletes the element at the given `index` from `tuple`.
  Raises an `ArgumentError` if `index` is negative or greater than
  or equal to the length of `tuple`. Index is zero-based.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, :baz}
      iex> Tuple.delete_at(tuple, 0)
      {:bar, :baz}

  """
  @spec delete_at(tuple, non_neg_integer) :: tuple
  def delete_at(tuple, index) do
    :erlang.delete_element(index + 1, tuple)
  end

  @doc """
  Converts a tuple to a list.

  Returns a new list with all the tuple elements.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, :baz}
      iex> Tuple.to_list(tuple)
      [:foo, :bar, :baz]

  """
  @spec to_list(tuple) :: list
  def to_list(tuple) do
    :erlang.tuple_to_list(tuple)
  end

  @doc """
  Defines a set of macros to create, access, and pattern match on
  a tuple.

  The name of the generated macros will be `name` (which has to be an
  atom). `kv` is a keyword list of `name: default_value` fields for the
  new tuple.

  The following macros are generated:

    * `name/0` to create a new tuple with default values for all fields
    * `name/1` to create a new tuple with the given fields and values,
      to get the zero-based index of the given field in a tuple or to
      convert the given tuple to a keyword list
    * `name/2` to update an existing tuple with the given fields and values
      or to access a given field in a given tuple

  All these macros are public macros (as defined by `defmacro`).

  See the "Examples" section for examples on how to use these macros.

  ## Examples

      defmodule Space do
        require Tuple
        Tuple.deftuple :point, [x: 0, y: 0, z: 0]
      end

  In the example above, a set of macros named `point` but with different
  arities will be defined to manipulate the underlying tuple.

      # Import the module to make the point macros locally available
      import Space

      # To create tuples
      tuple = point()     #=> {0, 0, 0}
      tuple = point(x: 7) #=> {7, 0, 0}

      # To get a field from the tuple
      point(tuple, :x) #=> 7

      # To update the tuple
      point(tuple, y: 9) #=> {7, 9, 0}

      # To get the zero-based index of the field in tuple
      point(:x) #=> 0

      # Convert a tuple to a keyword list
      point(tuple) #=> [x: 7, y: 0, z: 0]

  The generated macros can also be used in order to pattern match on tuples and
  to bind variables during the match:

      point() = tuple #=> {7, 0, 0}

      point(x: x) = tuple
      x #=> 7
  """
  defmacro deftuple(name, kv) do
    quote bind_quoted: [name: name, kv: kv] do
      fields = Tuple.__fields__(:deftuple, kv)

      defmacro(unquote(name)(args \\ [])) do
        Tuple.__access__(unquote(name), unquote(fields), args, __CALLER__)
      end

      defmacro(unquote(name)(tuple, args)) do
        Tuple.__access__(unquote(name), unquote(fields), tuple, args, __CALLER__)
      end
    end
  end

  @doc """
  Same as `deftuple/2` but generates private macros.
  """
  defmacro deftuplep(name, kv) do
    quote bind_quoted: [name: name, kv: kv] do
      fields = Tuple.__fields__(:deftuplep, kv)

      defmacrop(unquote(name)(args \\ [])) do
        Tuple.__access__(unquote(name), unquote(fields), args, __CALLER__)
      end

      defmacrop(unquote(name)(tuple, args)) do
        Tuple.__access__(unquote(name), unquote(fields), tuple, args, __CALLER__)
      end
    end
  end

  # Normalizes of tuple fields to have default values.
  @doc false
  def __fields__(type, fields) do
    :lists.map(fn
      {key, val} when is_atom(key) ->
        try do
          Macro.escape(val)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "invalid value for tuple field #{key}, " <> Exception.message(e)
        else
          val -> {key, val}
        end
      key when is_atom(key) ->
        {key, nil}
      other ->
        raise ArgumentError, "#{type} fields must be atoms, got: #{inspect other}"
    end, fields)
  end

  # Callback invoked from tuple/0 and tuple/1 macros.
  @doc false
  def __access__(atom, fields, args, caller) do
    cond do
      is_atom(args) ->
        index(atom, fields, args)
      Keyword.keyword?(args) ->
        create(atom, fields, args, caller)
      true ->
        case Macro.expand(args, caller) do
          {:{}, _, list} when length(list) == length(fields) ->
            tuple = List.to_tuple(list)
            Tuple.__keyword__(atom, fields, tuple)
          {_, _} = pair when length(fields) == 2 ->
            Tuple.__keyword__(atom, fields, pair)
          _ ->
            quote do: Tuple.__keyword__(unquote(atom), unquote(fields), unquote(args))
        end
    end
  end

  # Callback invoked from the tuple/2 macro.
  @doc false
  def __access__(atom, fields, tuple, args, caller) do
    cond do
      is_atom(args) ->
        get(atom, fields, tuple, args)
      Keyword.keyword?(args) ->
        update(atom, fields, tuple, args, caller)
      true ->
        msg = "expected arguments to be a compile time atom or keywords, got: #{Macro.to_string args}"
        raise ArgumentError, msg
    end
  end

  # Gets the index of field.
  defp index(atom, fields, field) do
    if index = find_index(fields, field, 0) do
      index - 1 # Convert to Elixir index
    else
      raise ArgumentError, "tuple #{inspect atom} does not have the key: #{inspect field}"
    end
  end

  # Creates a new tuple with the given default fields and keyword values.
  defp create(atom, fields, keyword, caller) do
    in_match = Macro.Env.in_match?(caller)
    keyword = apply_underscore(fields, keyword)

    {match, remaining} =
      Enum.map_reduce(fields, keyword, fn({field, default}, each_keyword) ->
        new_fields =
          case Keyword.fetch(each_keyword, field) do
            {:ok, value} -> value
            :error when in_match -> {:_, [], nil}
            :error -> Macro.escape(default)
          end

        {new_fields, Keyword.delete(each_keyword, field)}
      end)

    case remaining do
      [] ->
        {:{}, [], match}
      _  ->
        keys = for {key, _} <- remaining, do: key
        raise ArgumentError, "tuple #{inspect atom} does not have the key: #{inspect hd(keys)}"
    end
  end

  # Updates a tuple given by var with the given keyword.
  defp update(atom, fields, var, keyword, caller) do
    if Macro.Env.in_match?(caller) do
      raise ArgumentError, "cannot invoke update style macro inside match"
    end

    keyword = apply_underscore(fields, keyword)

    Enum.reduce keyword, var, fn({key, value}, acc) ->
      index = find_index(fields, key, 0)
      if index do
        quote do
          :erlang.setelement(unquote(index), unquote(acc), unquote(value))
        end
      else
        raise ArgumentError, "tuple #{inspect atom} does not have the key: #{inspect key}"
      end
    end
  end

  # Gets a tuple key from the given var.
  defp get(atom, fields, var, key) do
    index = find_index(fields, key, 0)
    if index do
      quote do
        :erlang.element(unquote(index), unquote(var))
      end
    else
      raise ArgumentError, "tuple #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  defp find_index([{k, _} | _], k, i), do: i + 1
  defp find_index([{_, _} | t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil

  # Returns a keyword list of the tuple
  @doc false
  def __keyword__(atom, fields, tuple) do
    if is_tuple(tuple) do
      values = Tuple.to_list(tuple)
      case join_keyword(fields, values, []) do
        kv when is_list(kv) ->
          kv
        expected_size ->
          msg = "expected argument to be a #{inspect atom} tuple of size #{expected_size}, got: #{inspect tuple}"
          raise ArgumentError, msg
      end
    else
      msg = "expected argument to be a literal atom, literal keyword or a #{inspect atom} tuple, got runtime: #{inspect tuple}"
      raise ArgumentError, msg
    end
  end

  # Returns a keyword list, or expected size on size mismatch
  defp join_keyword([{field, _default} | fields], [value | values], acc),
    do: join_keyword(fields, values, [{field, value} | acc])
  defp join_keyword([], [], acc),
    do: :lists.reverse(acc)
  defp join_keyword(rest_fields, _rest_values, acc),
    do: length(acc) + length(rest_fields) # expected size

  defp apply_underscore(fields, keyword) do
    case Keyword.fetch(keyword, :_) do
      {:ok, default} ->
        fields
        |> Enum.map(fn {k, _} -> {k, default} end)
        |> Keyword.merge(keyword)
        |> Keyword.delete(:_)
      :error ->
        keyword
    end
  end
end
