void main() {
    var acct1 = new Account();
    acct1.name = "Bruce Wayne";
    acct1.address = "101 Wayne Way, Gotham City 27001";
    acct1.balance = 500_000_000.99;
    var acct2 = new Account();
    acct2.name = "Selina Kyle";
    acct2.address = "15 Shorthair, Gotham City 27001";
    acct2.balance = 10032.89;

    IO.println("My account: " + acct1);
    IO.println("Your account: " + acct2);
}
