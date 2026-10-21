void main() {
    IO.println("Usage: Firstname Lastname<RET>Title Amount");
    var keyboard = new Scanner(System.in);
    var name = keyboard.nextLine();
    var jobTitle = keyboard.next();
    var emp = new Employee(name, jobTitle);
    var amount = keyboard.nextDouble();
    emp.cutCheck(amount);
    keyboard.close();
}
