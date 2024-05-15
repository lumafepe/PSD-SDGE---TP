package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AlbumLeaveCommand implements Command {

    final String regex = "album leave (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(AlbumLeave command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for leaving the album based on the provided album ID
    }

    private AlbumLeave parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new AlbumLeave(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record AlbumLeave(String albumId) {}
}
