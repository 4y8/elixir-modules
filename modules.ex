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

      def meow do
        "Meow!" <> Animals.Mammals.Canines.bark() <> breathe()
      end

      def scratch do
        "Scratching the post..." <> Animals.Mammals.Felines.breathe() # fails because breathe is private
      end
    end


  end

  defmodule Birds do
    defmodule Songbirds do
      def sing do
        "Chirp chirp!"
      end

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
