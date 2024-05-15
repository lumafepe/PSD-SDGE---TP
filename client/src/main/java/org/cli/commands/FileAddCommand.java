package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FileAddCommand implements Command {

    final String regex = "file add (.+) (.+)";
    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(FileAdd command, Object state /* TODO: Tipar direito */) {
        //TODO
    }

    private FileAdd parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if(matcher.find()) {
            return new FileAdd(matcher.group(1), matcher.group(2));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record FileAdd(String path, String name) {}
}
