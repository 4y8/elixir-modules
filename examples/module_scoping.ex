defmodule Animals do
  defmodule Mammals do
    defmodule Canines do
      def bark do
        "Woof!"
      end

      def fetch do
        "Fetching the ball..."
      end
    end

    defmodule Felines do
      defp breathe do
        "Breathing air..."
      end

      IO.inspect __ENV__.aliases

      def meow do
        "Meow!" <> Animals.Mammals.Canines.bark() <> breathe()
        "Meow!" <> Mammals.Canines.bark() <> breathe()         # should fail since Mammals is not top-level
        "Meow!" <> Canines.bark() <> breathe()         # should fail since Canine is not in the same module

      end

      def scratch do
        "Scratching the post..." #<> Animals.Mammals.Felines.breathe() # fails because breathe is private
      end
    end
    def test do
      "Testing..." <> Animals.Mammals.Felines.meow()
    end

  end

  defmodule Birds do
    defmodule Songbirds do
      defmodule Canines do
        def bird_bark do
          "Tweet!"
        end
      end
      def sing do
        "Chirp chirp!"
      end
      IO.inspect __ENV__.aliases

      def fly do
        "Flying through the sky..."
      end
    end

    def lay_eggs do
      "Laying eggs..."
    end
  end

  def classify do
    "Classifying animals..."
  end
end

defmodule Greetings do
  defmodule Languages do
    defmodule English do
      def hello do
        "Hello, World!"
      end

      def goodbye do
        "Goodbye!"
      end
    end

    defmodule Spanish do
      def hola do
        "¡Hola, Mundo!"
      end

      def adios do
        "¡Adiós!"
      end
    end

    defmodule French do
      def bonjour do
        "Bonjour, Monde!"
      end

      def au_revoir do
        "Au revoir!"
      end
    end
  end

  def greet(name) do
    "Greeting #{name}..."
  end
end
