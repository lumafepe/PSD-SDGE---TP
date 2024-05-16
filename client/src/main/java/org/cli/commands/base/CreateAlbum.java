package org.cli.commands.base;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CreateAlbum implements Command {

    public final String re = "create (\\S+)|create help";
    private final String prompt;

    public CreateAlbum(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private CreateCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new CreateCommand(matcher.group(1));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("create help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        CreateCommand album = this.parseCommand(command);
        if (album == null) {
            System.out.printf(this.prompt + "[-] could not parse create command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send("/createAlbum " + album.albumName);

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to create album '%s': '%s'\n", album.albumName, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] album '%s' successfully created\n", album.albumName);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] create <album_name> : create a new album\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record CreateCommand(String albumName) {}
}
