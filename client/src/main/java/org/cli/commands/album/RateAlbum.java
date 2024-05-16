package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.cli.commands.base.EditAlbum;
import org.messages.central.AlbumMessage;
import org.messages.central.File;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RateAlbum implements Command {

    public final String re = "rate (\\S+) (\\d+)|rate help";

    private final String prompt;
    private final String username;

    public RateAlbum(String prompt, String username) {
        this.prompt = prompt.strip();
        this.username = username;
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private RateCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new RateCommand(matcher.group(1), matcher.group(2));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("rate help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        RateCommand rate = this.parseCommand(command);
        if (rate == null) {
            System.out.printf(this.prompt + "[-] could not parse rate command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/rate %s %s %s", this.username, rate.file, rate.value));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to rate file '%s' with value '%s': '%s'\n", rate.file, rate.value, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] rated '%s' with value '%s'\n", rate.file, rate.value);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] rate <file> <value> : rate a file\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record RateCommand(String file, String value) {}
}
