public record Employee
(String name,
String jobTitle)
{
    public void cutCheck(double amount) {
        IO.print("Pay to the order of " + name);
        IO.print(" (" + jobTitle + ")");
        IO.print(" ***$");
        IO.println(String.format("%,.2f", amount));
    }
}
