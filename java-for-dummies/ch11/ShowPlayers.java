void main(String args[]) throws IOException {
    var filename = args[0];
    IO.println("Reading file: " + filename);
    var dat = new Scanner(new File(filename));
    showHeader();
}

void showHeader() {
    IO.print("Player");
    IO.println(String.format("%45s", "Batting Average"));
    IO.print("------");
    IO.println(String.format("%45s", "---------------"));
}
