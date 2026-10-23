void main(String[] args) {
    var total = Arrays.stream(args)
                .map(it -> Double.parseDouble(it))
                .reduce(0.0, (a, b) -> a + b);
    IO.println("Total: " + total);
}
