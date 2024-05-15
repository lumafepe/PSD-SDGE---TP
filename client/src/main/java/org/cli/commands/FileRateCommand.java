package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FileRateCommand implements Command {

    final String regex = "file rate (\\S+) ([1-5])";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(FileRate command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for rating the file
    }

    private FileRate parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new FileRate(matcher.group(1), Integer.parseInt(matcher.group(2)));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record FileRate(String filename, int rating) {}
}
