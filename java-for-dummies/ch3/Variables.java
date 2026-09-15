import static java.lang.IO.print;
import static java.lang.IO.println;

class Variables {
    void main() {
        var weightOfAPerson = 150;
        var elevatorWeightLimit = 1400;
        var numberOfPeople = elevatorWeightLimit / weightOfAPerson;

        print("You can fit ");
        print(numberOfPeople);
        println(" people in the elevator.");
    }
}
