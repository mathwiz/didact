import static java.lang.IO.println;

class TextBlock {
    void main() {
        println("""
The line with the three quote marks must be blank to the end of line.
        This is indended.
This is not indended.
                This is furhter indented.
                The last line of text can end with the closing three quote marks.""");
    }
}
