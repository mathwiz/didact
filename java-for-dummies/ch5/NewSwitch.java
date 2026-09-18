import static java.lang.IO.println;
import static java.lang.IO.readln;

class NewSwitch {
    void main() {
        int choice = Integer.parseInt(readln("Choose a number: "));
        switch (choice) {
            case 1 -> println("You picked the first");
            case 2 -> println("The second is the choice");
            case 3 -> println("Three is your choice");
            default -> println("This is not a valid choice");
        }
        println("Thanks for choosing!");
    }
}
