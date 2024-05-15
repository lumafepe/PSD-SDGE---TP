package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FileRemoveCommand implements Command {

    final String regex = "file remove (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(FileRemove command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for removing the file based on its hash
    }

    private FileRemove parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if(matcher.find()) {
            return new FileRemove(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record FileRemove(String hash) {}
}
