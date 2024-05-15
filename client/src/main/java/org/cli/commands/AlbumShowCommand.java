package org.cli.commands;

public class AlbumShowCommand implements Command {

    final String command = "album";

    @Override
    public boolean matches(String input) {
        return input.trim().equalsIgnoreCase(command);
    }

    @Override
    public void execute(String input, Object state) {
        // TODO: Implement the logic for displaying the album
    }
}
