void main() {
    IO.println("Usage: Firstname Lastname<RET>Title Amount");
    var name = IO.readln("Name? ");
    var jobTitle = IO.readln("Job Title? ");
    var emp = new Employee(name, jobTitle);
    var amount = Double.parseDouble(IO.readln("Pay? "));
    emp.cutCheck(amount);
}
