public record Temperature (double value, TempScale scale) {
    public String formatted() {
        return String.format("%5.2f\u00B0 ", value) + scale;
    }
}
