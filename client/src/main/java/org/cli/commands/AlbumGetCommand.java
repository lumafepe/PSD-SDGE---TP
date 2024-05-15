package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AlbumGetCommand implements Command {

    final String regex = "album get (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(AlbumGet command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for retrieving the album based on the album ID
    }

    private AlbumGet parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new AlbumGet(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record AlbumGet(String albumId) {}
}
