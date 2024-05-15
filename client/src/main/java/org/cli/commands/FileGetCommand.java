package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FileGetCommand implements Command {

    final String regex = "file get (\\S+) (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(FileGet command, Object state /* TODO: Tipar direito */) {
        //TODO: Implement the logic for fetching the file based on its hash and saving it to the destination path
    }

    private FileGet parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if(matcher.find()) {
            return new FileGet(matcher.group(1), matcher.group(2));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record FileGet(String hash, String destination) {}
}