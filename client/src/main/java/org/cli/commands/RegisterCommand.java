package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegisterCommand implements Command {

    final String regex = "register (\\S+) (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(RegisterUser command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for registering a new user with the provided username and password
    }

    private RegisterUser parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new RegisterUser(matcher.group(1), matcher.group(2));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record RegisterUser(String username, String password) {}
}
