void main(String[] args) {
    var total = Arrays.stream(args)
                .map(it -> Double.parseDouble(it))
                .reduce(0.0, (a, b) -> a + Math.pow(b, 2.0));
    IO.println("Total: " + total);
}
