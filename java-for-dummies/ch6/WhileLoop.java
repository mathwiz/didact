import java.util.Random;
import static java.lang.IO.println;
import static java.lang.IO.print;
import static java.lang.IO.readln;

class WhileLoop {
    void main() {
        int guesses = 0;
        int randomNumber = new Random().nextInt(10) + 1;

        println("""
        ****************************
        Welcome to the Guessing Game
        ****************************
        """);

        String input = readln("Pick a number from 1 to 10: ");
        int guess = Integer.parseInt(input);
        guesses++;

        while (guess != randomNumber) {
            println();
            println("Try again...");
            input = readln("Pick a number from 1 to 10: ");
            guess = Integer.parseInt(input);
            guesses++;
        };

        println("You win after " + guesses + " guesses");
    }
}
