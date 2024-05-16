package org.cli.commands.base;

import org.cli.Sender;
import org.cli.commands.Command;
import org.cli.commands.album.*;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class EditAlbum implements Command {

    public final String re = "edit (\\S+)|edit help";
    private String prompt;
    private String album;
    private final String username;

    private List<Command> subcommands;

    public EditAlbum(String prompt, String username) {
        this.prompt = prompt.strip();
        this.username = username;
    }

    private EditCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new EditCommand(matcher.group(1));
        }
        return null;
    }

    private void setSubCommands() {
        this.subcommands = Arrays.asList(
                new LeaveAlbum(this.prompt, this.album),
                new ShowAlbum(this.prompt, this.album),
                new RateAlbum(this.prompt, this.username),
                new DownloadFile(this.prompt),
                new UploadFile(this.prompt),
                new DeleteFile(this.prompt),
                new Chat(this.prompt),
                new AddUser(this.prompt),
                new RemoveUser(this.prompt)
        );
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("edit help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        EditCommand album = this.parseCommand(command);
        if (album == null) {
            System.out.printf(this.prompt + "[-] could not parse edit command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/editAlbum %s", album.albumName));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to select album '%s' for edit: '%s'\n", album.albumName, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] editing album '%s'\n", album.albumName);

        this.prompt = String.format("%s[%s]", this.prompt, album.albumName);
        this.album = album.albumName;
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

            if (c.equalsIgnoreCase("leave")) {
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
        return this.prompt + "[?] edit <album_name> : start an editing session on a album\n";
    }

    @Override
    public String help() {
        return this.prompt + "[?] available commands for edit: leave, show, rate, upload, download, delete, add, remove, chat\n";
    }

    private record EditCommand(String albumName) {}
}
