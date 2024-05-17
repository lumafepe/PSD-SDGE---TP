package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DownloadFile implements Command {

    public final String re = "download (\\S+) (\\S+)|download help";

    private final String prompt;

    public DownloadFile(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private DownloadCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new DownloadCommand(matcher.group(1), matcher.group(2));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("download help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        DownloadCommand file = this.parseCommand(command);
        if (file == null) {
            System.out.printf(this.prompt + "[-] could not parse download command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        System.out.printf(this.prompt + "[!] downloading file '%s', this might take a while...\n", file.filename);
        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/getFile %s %s", file.filename, file.destination));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to download file '%s' to '%s': '%s'\n", file.filename, file.destination, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] downloaded file '%s' to '%s'\n", file.filename, file.destination);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] download <file_name> <destination> : download a file to a destination\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record DownloadCommand(String filename, String destination) {}
}
