package org.client;

import org.cli.CLI;

import java.util.ArrayList;
import java.util.List;
import org.cli.commands.*;

public class Main {
    private static List<Command> getCommands() {
        List<Command> commandList = new ArrayList<>();

        // Adding instances of all command classes
        commandList.add(new AlbumCreateCommand());
        commandList.add(new AlbumEditCommand());
        commandList.add(new AlbumGetCommand());
        commandList.add(new AlbumLeaveCommand());
        commandList.add(new AlbumListCommand());
        commandList.add(new ChatBroadcastCommand());
        commandList.add(new FileGetCommand());
        commandList.add(new FileAddCommand());
        commandList.add(new FileRateCommand());
        commandList.add(new LoginCommand());
        commandList.add(new LogoutCommand());
        commandList.add(new RegisterCommand());
        commandList.add(new AlbumShowCommand());
        commandList.add(new UserAddCommand());
        commandList.add(new UserRemoveCommand());

        return commandList;
    }

    public static void main(String[] args) {
        CLI cli = new CLI(getCommands(), null);
        cli.run();
    }
}
