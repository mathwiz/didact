void main () {
    var temp = new Temperature(-500, TempScale.CELSIUS);
    IO.println(temp.formatted());
    IO.println(String.format("%f %s", temp.value(), temp.scale()));
    temp = new Temperature(0.0, TempScale.CELSIUS);
    IO.println(temp);

    var temp2 = new Temperature(0.0, TempScale.CELSIUS);
    var temp3 = new Temperature(32.0, TempScale.FAHRENHEIT);
    IO.println(temp.formatted() + " equals " + temp2.formatted() + "?");
    IO.println(temp.equals(temp2));
    IO.println(temp.formatted() + " equals " + temp3.formatted() + "?");
    IO.println(temp.equals(temp3));
}
