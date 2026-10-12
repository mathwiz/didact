void main() {
    var acct1 = new Account();
    acct1.name = "Bruce Wayne";
    acct1.address = "101 Wayne Way, Gotham City 27001";
    acct1.balance = 500_000_000.99;
    var acct2 = new Account();
    acct2.name = "Selina Kyle";
    acct2.address = "15 Shorthair, Gotham City 27001";
    acct2.balance = 10032.89;

    IO.println("My account: " + acct1 + " has " + dollarize(acct1.balance));
    IO.println("Plus " + dollarize(acct1.getInterest(5.25)) + " interest");
    IO.println("Your account: " + acct2 + " has " + dollarize(acct2.balance));
    IO.println("Plus " + dollarize(acct2.getInterest(7.00)) + " interest");
}

String dollarize(double val) {
    return String.format("$%.2f", val);
}
