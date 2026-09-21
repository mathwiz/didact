import static java.lang.IO.println;
import static java.lang.IO.readln;

class NewSwitchString {
    void main() {
        String reply = readln("Are you trustworthy? ");

        String response = switch (reply.toLowerCase()) {
            case "yes", "y", "ok" -> "Cool beans!";
            case "maybe", "sure" -> "Please confirm.";
            case "no", "n", "nah" -> "That's disapointing.";
            default -> "I don't know what that means.";
        };

        println(response);
    }
}
