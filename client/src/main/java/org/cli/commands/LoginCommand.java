package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LoginCommand implements Command {

    final String regex = "login (\\S+) (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(LoginUser command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for logging in the user with the provided username and password
    }

    private LoginUser parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new LoginUser(matcher.group(1), matcher.group(2));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record LoginUser(String username, String password) {}
}
