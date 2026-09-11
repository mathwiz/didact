import static java.lang.IO.print;
import static java.lang.IO.println;

class Variables {
    void main() {
        int weightOfAPerson;
        int elevatorWeightLimit;
        int numberOfPeople;

        weightOfAPerson = 150;
        elevatorWeightLimit = 1400;
        numberOfPeople = elevatorWeightLimit / weightOfAPerson;

        print("You can fit ");
        print(numberOfPeople);
        println(" people in the elevator.");
    }
}
