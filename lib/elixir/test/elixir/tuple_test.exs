Code.require_file "test_helper.exs", __DIR__

defmodule TupleTest do
  use ExUnit.Case, async: true

  doctest Tuple

  require Tuple

  test "elem" do
    assert elem({:a, :b, :c}, 1) == :b
  end

  test "put elem" do
    assert put_elem({:a, :b, :c}, 1, :d) == {:a, :d, :c}
  end

  test "keywords" do
    assert {1, 2, three: :four} == {1, 2, [three: :four]}
  end

  test "optional comma" do
    assert {1} == {1,}
    assert {1, 2, 3} == {1, 2, 3,}
  end

  test "partial application" do
    assert (&{&1, 2}).(1) == {1, 2}
    assert (&{&1, &2}).(1, 2) == {1, 2}
    assert (&{&2, &1}).(2, 1) == {1, 2}
  end

  # Tuple module
  # We check two variants due to inlining.

  test "duplicate" do
    assert Tuple.duplicate(:foo, 0) == {}
    assert Tuple.duplicate(:foo, 3) == {:foo, :foo, :foo}

    mod = Tuple
    assert mod.duplicate(:foo, 0) == {}
    assert mod.duplicate(:foo, 3) == {:foo, :foo, :foo}
  end

  test "insert at" do
    assert Tuple.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}
  end

  test "append" do
    assert Tuple.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}
  end

  test "delete at" do
    assert Tuple.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}

    mod = Tuple
    assert mod.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}
  end

  Tuple.deftuple :point, x: 0, y: 0, z: 0
  Tuple.deftuplep :timestamp, [:date, :time]  # defining two-element tuple, isomorphic in AST

  test "tuples macros" do
    tuple = point()
    assert tuple == {0, 0, 0}

    tuple = point(x: 1, y: 2)
    assert tuple == {1, 2, 0}

    tuple = point(tuple, y: 0, z: 3)
    assert tuple == {1, 0, 3}

    assert point(tuple, :x) == 1
    assert point(tuple, :y) == 0
    assert point(tuple, :z) == 3

    assert point(:x) == 0
    assert point(:y) == 1
    assert point(:z) == 2

    assert elem(tuple, point(:x)) == 1
    assert elem(tuple, point(:y)) == 0
    assert elem(tuple, point(:z)) == 3

    point(x: x, y: y, z: z) = tuple
    assert {x, y, z} == {1, 0, 3}
  end

  test "two-element tuples macros" do
    tuple = timestamp()
    assert tuple == {nil, nil}

    tuple = timestamp(date: :foo, time: :bar)
    assert tuple == {:foo, :bar}

    tuple = timestamp(tuple, date: :bar, time: :baz)
    assert tuple == {:bar, :baz}

    assert timestamp(tuple, :date) == :bar
    assert timestamp(tuple, :time) == :baz

    assert timestamp(:date) == 0
    assert timestamp(:time) == 1

    timestamp(date: date, time: time) = tuple
    assert {date, time} == {:bar, :baz}

    assert timestamp(timestamp()) == [date: nil, time: nil]
    assert timestamp(tuple) == [date: :bar, time: :baz]
  end

  test "tuples with duplicated arguments (always first win)" do
    assert point(x: 1, y: 2, z: 3, x: 111, y: 222, z: 333) == {1, 2, 3}
    assert point(x: 1,       z: 3, x: 111,         z: 333) == {1, 0, 3}
  end

  test "tuples with no defaults" do
    tuple = timestamp()
    assert timestamp(tuple, :date) == nil
    assert timestamp(tuple, :time) == nil

    tuple = timestamp(date: :foo, time: :bar)
    assert timestamp(tuple, :date) == :foo
    assert timestamp(tuple, :time) == :bar
  end

  test "tuples with default values" do
    tuple = point(_: nil, y: 2)
    assert tuple == {nil, 2, nil}

    assert match?(point(_: _), point())
    refute match?(point(_: "NAN"), point())

    tuple = point(point(), _: nil, y: 2)
    assert tuple == {nil, 2, nil}
  end

  Tuple.deftuplep :defaults,
    struct: ~D[2016-01-01],
    map: %{},
    tuple_zero: {},
    tuple_one: {1},
    tuple_two: {1, 2},
    tuple_three: {1, 2, 3},
    list: [1, 2, 3],
    call: MapSet.new,
    string: "abc",
    binary: <<1, 2, 3>>,
    charlist: 'abc'

  test "tuples with literal defaults" do
    assert defaults(defaults()) == [
      struct: ~D[2016-01-01],
      map: %{},
      tuple_zero: {},
      tuple_one: {1},
      tuple_two: {1, 2},
      tuple_three: {1, 2, 3},
      list: [1, 2, 3],
      call: MapSet.new,
      string: "abc",
      binary: <<1, 2, 3>>,
      charlist: 'abc'
    ]
    assert defaults(defaults(), :struct) == ~D[2016-01-01]
    assert defaults(defaults(), :map) == %{}
    assert defaults(defaults(), :tuple_zero) == {}
    assert defaults(defaults(), :tuple_one) == {1}
    assert defaults(defaults(), :tuple_two) == {1, 2}
    assert defaults(defaults(), :tuple_three) == {1, 2, 3}
    assert defaults(defaults(), :list) == [1, 2, 3]
    assert defaults(defaults(), :call) == MapSet.new
    assert defaults(defaults(), :string) == "abc"
    assert defaults(defaults(), :binary) == <<1, 2, 3>>
    assert defaults(defaults(), :charlist) == 'abc'
  end

  test "tuples with dynamic arguments" do
    assert point(point()) == [x: 0, y: 0, z: 0]
    tuple = point()
    assert point(tuple)   == [x: 0, y: 0, z: 0]

    invalid = {1, 2}
    msg = "expected argument to be a :point tuple of size 3, got: {1, 2}"
    assert_raise ArgumentError, msg, fn ->
      point(invalid)
    end

    invalid = {1, 2, 3, 4}
    msg = "expected argument to be a :point tuple of size 3, got: {1, 2, 3, 4}"
    assert_raise ArgumentError, msg, fn ->
      point(invalid)
    end
  end

  test "tuples visibility" do
    assert macro_exported?(__MODULE__, :point, 0)
    assert macro_exported?(__MODULE__, :point, 1)
    assert macro_exported?(__MODULE__, :point, 2)
    refute macro_exported?(__MODULE__, :timestamp, 0)
  end
end
