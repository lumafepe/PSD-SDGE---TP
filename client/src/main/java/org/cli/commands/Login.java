package org.cli.commands;

import org.cli.Sender;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.Map;
import java.util.Scanner;

public class Login implements Command {

    private Sender s = Sender.getInstance();
    private Map<String, Command> commands;
    private String username;

    private final String prompt;

    public Login(String prompt) {
        this.prompt = prompt;
    }

    private Credentials parseLogin(String command) {
        String[] parts = command.split(" ");
        if (parts.length != 2) {
            return null;
        }

        this.username = parts[0];
        return new Credentials(parts[0], parts[1]);
    }

    @Override
    public void execute(String command) {

        if (command.equals("help") || command.equals("h") || command.isEmpty()) {
            // todo: print description and usage
            return;
        }

        Credentials cred = this.parseLogin(command);
        if (cred == null) {
            // todo: echo error
            return;
        }

        Message reply = s.send("login" + command);
        if (!reply.getType().equals(Type.SUCESIUM)) {
            // todo: echo error
            return;
        }

        String prompt = this.prompt + String.format("[%s] ", this.username);
        System.out.print(prompt);

        Scanner scanner = new Scanner(System.in);
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            System.out.print("command = " + line);
        }
    }

    @Override
    public String description() {
        return "";
    }

    @Override
    public String usage() {
        return "";
    }

    private record Credentials(String username, String password) {}
}

/*
$ ash
[home] help
[*] Here's a list of commands:
      login - login into the application
      register - register a new account

[home] login help
[*] Usage of 'login':
      login <username> <password>
      login help

[home] login guilherme my-password
[*] Logged in as "guilherme"

[home][guilherme] help
[*] Here's a list of commands:
    logout - logout of the application
    list - list all albums
    edit - select and edit an album
    exit - logout and exit of the application
    ...

[home][guilherme] list
[*] Albums:
      "Album 1"
      "Album 2"

[home][guilherme] edit "Album 1"
[*] "Album 1" selected for edit

[home][guilherme][Album 1] help
[*] Here's a list of commands:
      list - list files from the album
      select - select a file to act
      upload - upload a file
      download - download a file
      back - stop editing current album

[home][guilherme][Album 1] list
[*] Files:
    .bashrc
    .fish

[home][guilherme][Album 1] select .bashrc
[*] File ".bashrc" selected

[home][guilherme][Album 1][.bashrc] help
[*] Here's a list of commands:
      rate - rate the file
      back - return to album
*/