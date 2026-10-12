import java.lang.IO;

class AccountExample {
    void main() {
        var acct1 = new Account();
        acct1.name = "Barry Allen";
        acct1.address = "123 Main, Central City 10023";
        acct1.balance = 127.15;
        var acct2 = new Account();
        acct2.name = "Caitlin Snow";
        acct2.address = "270 Wintergreen, Central City 10023";
        acct2.balance = 110032.89;

        IO.println("My account: " + acct1);
        IO.println("Your account: " + acct2);
    }

}
