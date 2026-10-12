public class Account {
    String name;
    String address;
    double balance;

    public String toString() {
        return name + " (" + address + ") has $" + balance;
    }
}
