package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UserRemoveCommand implements Command {

    final String regex = "user remove (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(UserRemove command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for removing the user based on the username
    }

    private UserRemove parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new UserRemove(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record UserRemove(String username) {}
}
