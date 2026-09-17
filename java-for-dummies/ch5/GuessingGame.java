import java.util.Random;
import static java.lang.IO.println;
import static java.lang.IO.readln;

class GuessingGame {
    void main() {
        String input = readln("Pick a number from 1 to 10: ");
        var inputNum = Integer.parseInt(input);
        var randomNum = new Random().nextInt(10) + 1;

        if (inputNum == randomNum) {
            println("************");
            println("* You Win! *");
            println("************");
        } else {
            println("Sorry, you lose.");
            println("The correct number was " + randomNum);
            println("Thank you for playing!");
        }
        println("done");
    }
}
