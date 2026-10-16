public record Temperature
(double value,
TempScale scale)
{
    public Temperature(double value, TempScale scale) {
        this.value = Math.max(value,
        switch (scale) {
            case CELSIUS -> -273.15;
            case FAHRENHEIT -> -459.6;
            case KELVIN, RANKINE -> 0.0;
        });
        this.scale = scale;
    }

    double toCelcius() {
        return switch (scale) {
            case CELSIUS -> value;
            case FAHRENHEIT -> (value - 32.0) * 5.0 / 9.0;
            case RANKINE -> (value - 491.67) * 5.0 / 9.0;
            case KELVIN -> value - 273.15;
        };
    }

    public boolean equals(Temperature other) {
        var myVal = this.toCelcius();
        var otherVal = other.toCelcius();
        return myVal == otherVal;
    }

    public String formatted() {
        return String.format("%5.2f\u00B0 ", value) + scale;
    }
}
