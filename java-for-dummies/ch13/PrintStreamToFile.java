void main(String args[]) throws IOException {
    var filename = args[0];
    IO.println("Writing to file: " + filename);
    writeData(filename, getData());
}

void writeData(String filename, int[] data) throws IOException {
    var out = new PrintStream(filename);
    for (int i=0; i < data.length; i++) {
        out.println(data[i]);
    }
}

int[] getData() {
    var keyboard = new Scanner(System.in);
    IO.print("How many scores? ");
    int size = keyboard.nextInt();
    int[] scores = new int[size];
    IO.println("Enter " + size + " scores:");
    for (int i=0; i < size; i++) {
        scores[i] = keyboard.nextInt();
    }
    Arrays.sort(scores);
    return scores;
}
