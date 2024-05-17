package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DeleteFile implements Command {

    public final String re = "delete (\\S+)|download help";

    private final String prompt;

    public DeleteFile(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private DeleteCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new DeleteCommand(matcher.group(1));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("delete help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        DeleteCommand file = this.parseCommand(command);
        if (file == null) {
            System.out.printf(this.prompt + "[-] could not parse delete command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/removeFile %s", file.filename));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to delete file '%s': '%s'\n", file.filename, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] deleted file '%s'\n", file.filename);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] delete <file_name> : delete a file from the album\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record DeleteCommand(String filename) {}
}
