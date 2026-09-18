import javax.swing.JOptionPane;

class Password {
    void main() {
        String username = dialog("Username");
        String password = dialog("Password");

        if (username != null && password != null
            &&
            (
                (username.equals("ylee") && password.equals("fubar"))
                ||
                (username.equals("hitler") && password.equals("kampf"))
            )
            )
        {
                JOptionPane.showMessageDialog(null, "You're in!");
        }
        else
        {
                JOptionPane.showMessageDialog(null, "Who are you!");
        }
    }

    String dialog(String param) {
        return JOptionPane.showInputDialog(param);
    }
}
