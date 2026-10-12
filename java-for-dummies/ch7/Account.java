public class Account {
    String name;
    String address;
    double balance;

    public double getInterest(double rate) {
        return balance * rate / 100.0;
    }

    public String toString() {
        return name + " (" + address + ") has $" + balance;
    }
}
