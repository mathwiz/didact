import static java.lang.IO.println;
import static java.lang.IO.readln;

class NewSwitchString {
    void main() {
        String reply = readln("Are you trustworthy? ");
        switch (reply.toLowerCase()) {
            case "yes", "y", "ok" -> println("Cool beans!");
            case "maybe", "sure" -> println("Please confirm.");
            case "no", "n", "nah" -> println("That's disapointing.");
            default -> println("I don't know what that means.");
        }
    }
}
