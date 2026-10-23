void main(String args[]) throws IOException {
    var filename = args[0];
    var num = Integer.parseInt(args[1]);
    IO.println("Reading " + num + " records from file: " + filename);
    var dat = new Scanner(new File(filename));
    showHeader();
    doRecords(dat, num);
    dat.close();
}

void doRecords(Scanner records, int number) {
    for (int i=0; i<number; i++) {
        var player = new Player(records.nextLine(),
                                records.nextDouble());
        records.nextLine();
        IO.print(String.format("%-40s", player.name()));
        IO.print(String.format("%.3f", player.average()));
        IO.println();
    }
}

void showHeader() {
    IO.print("Player");
    IO.println(String.format("%45s", "Batting Average"));
    IO.print("------");
    IO.println(String.format("%45s", "---------------"));
}
