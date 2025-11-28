defmodule Outer do
  @moduledoc """
  The outermost module containing nested modules.
  """
  def test do
    "This is a test function in the Outer module."
  end

  defmodule Middle do
    @moduledoc """
    A module nested inside Outer.
    """

    def greet do
      "Hello from Middle module"
    end

    def test do
      """
      Calling outer module:
      - #{Outer.test()}
      - #{Outer.greet()}
      """
    end

    defmodule Inner do
      @moduledoc """
      The innermost module, nested inside Middle.
      """

      def greet do
        "Hello from Inner module"
      end

      def call_all do
        """
        Calling all modules:
        - #{Outer.Middle.greet()}
        - #{Outer.Middle.Inner.greet()}
        """
      end
    end
  end

  def greet do
    Middle.Inner.call_all()
    "Hello from Outer module"
  end

end
