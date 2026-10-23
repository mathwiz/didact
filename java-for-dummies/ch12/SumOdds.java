void main(String[] args) {
    var total = Arrays.stream(args)
                .map(it -> Integer.parseInt(it))
                .filter(it -> it % 2 == 1)
                .reduce(0, (a, b) -> a + b);
    IO.println("Total: " + total);
}
