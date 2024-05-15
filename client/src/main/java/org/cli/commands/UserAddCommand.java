package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UserAddCommand implements Command {

    final String regex = "user add (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(UserAdd command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for adding the user based on the username
    }

    private UserAdd parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new UserAdd(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record UserAdd(String username) {}
}
