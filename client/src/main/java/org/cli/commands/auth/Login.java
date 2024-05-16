package org.cli.commands.auth;

import org.cli.Sender;
import org.cli.commands.Command;
import org.cli.commands.base.CreateAlbum;
import org.cli.commands.base.EditAlbum;
import org.cli.commands.base.ListAlbum;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Login implements Command {

    public final String re = "login (\\S+) (\\S+)|login help";
    private String prompt;
    private String username;

    private List<Command> subcommands;

    public Login(String prompt) {
        this.prompt = prompt.strip();
    }

    private LoginCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new LoginCommand(matcher.group(1), matcher.group(2));
        }
        return null;
    }

    private void setSubCommands() {
         this.subcommands = Arrays.asList(
                new Logout(this.prompt),
                new CreateAlbum(this.prompt),
                new EditAlbum(this.prompt, this.username),
                new ListAlbum(this.prompt)
        );
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("login help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        LoginCommand credentials = this.parseCommand(command);
        if (credentials == null) {
            System.out.printf(this.prompt + "[-] could not parse login credentials from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/login %s %s", credentials.username, credentials.password));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to log in: '%s'\n", reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] user '%s' logged in\n", credentials.username);

        this.prompt = String.format("%s[%s]", this.prompt, credentials.username);

        this.username = credentials.username;
        this.setSubCommands();
        Scanner sc = new Scanner(System.in);

        System.out.print(this.prompt);
        while (sc.hasNextLine()) {

            boolean success = false;
            String c = sc.nextLine();

            if (c.equalsIgnoreCase("help")) {
                System.out.print(this.help());
                System.out.print(this.prompt);
                continue;
            }

            if (c.equalsIgnoreCase("logout")) {
                this.subcommands.getFirst().execute(c);
                break;
            }

            for (Command cmd : this.subcommands) {
                if (cmd.matches(c)) {
                    cmd.execute(c);
                    success = true;
                    break;
                }
            }

            if (!success) {
                System.out.printf(this.prompt + "[-] unknown command '%s'%n", c);
                System.out.print(this.help());
            }

            System.out.print(this.prompt);
        }
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] login <username> <password> : log into the application\n";
    }

    @Override
    public String help() {
        return this.prompt + "[?] available commands for login: create, edit, list, logout\n";
    }

    private record LoginCommand(String username, String password) {}
}
