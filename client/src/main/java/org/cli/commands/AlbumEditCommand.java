package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AlbumEditCommand implements Command {

    final String regex = "album edit (\\S+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(AlbumEdit command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for editing the album name based on the provided album ID and new name
    }

    private AlbumEdit parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new AlbumEdit(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record AlbumEdit(String albumName) {}
}
