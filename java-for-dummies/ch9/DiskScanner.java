void main(String[] args) throws IOException {
    var filename = args[0];
    IO.println("Reading file " + filename);
    var input = new Scanner(new File(filename));
    for (int i=0; i < 3; i++) {
        var name = input.nextLine();
        var jobTitle = input.nextLine();
        var emp = new Employee(name, jobTitle);
        var amount = input.nextDouble();
        emp.cutCheck(amount);
        input.nextLine();
    }
    input.close();
}
