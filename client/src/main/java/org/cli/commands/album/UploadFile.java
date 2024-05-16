package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UploadFile implements Command {

    public final String re = "upload (\\S+) (\\S+)|upload help";

    private final String prompt;

    public UploadFile(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private UploadCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new UploadCommand(matcher.group(1), matcher.group(2));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("upload help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        UploadCommand file = this.parseCommand(command);
        if (file == null) {
            System.out.printf(this.prompt + "[-] could not parse upload command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/addFile %s %s", file.filename, file.filepath));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to upload file '%s' at '%s': '%s'\n", file.filename, file.filepath, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] uploaded file '%s'\n", file.filename);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] upload <file_name> <file_path> : upload a file into the album\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record UploadCommand(String filename, String filepath) {}
}
