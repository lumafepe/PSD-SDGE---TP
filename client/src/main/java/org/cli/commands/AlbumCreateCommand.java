package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AlbumCreateCommand implements Command {

    final String regex = "album create (.+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(AlbumCreate command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for creating a new album with the provided name
    }

    private AlbumCreate parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new AlbumCreate(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record AlbumCreate(String albumName) {}
}
