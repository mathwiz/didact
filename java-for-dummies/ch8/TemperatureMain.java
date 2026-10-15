void main () {
    var temp = new Temperature(26.2, TempScale.CELSIUS);
    IO.println(temp.formatted());
    IO.println(String.format("%f %s", temp.value(), temp.scale()));

    IO.println();
    temp = new Temperature(273.53, TempScale.KELVIN);
    IO.println(temp);
}
