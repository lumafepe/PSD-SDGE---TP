package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.AlbumMessage;
import org.messages.central.File;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Pattern;

public class ShowAlbum implements Command {

    public final String re = "show|show help";

    private final String prompt;
    private final String ctx;

    public ShowAlbum(String prompt, String ctx) {
        this.prompt = prompt.strip();
        this.ctx = ctx;
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("show help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send("/getAlbum " + this.ctx);

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to show album '%s': '%s'\n", this.ctx, reply.getErrorMessage().getMessage());
            return;
        }
        AlbumMessage album = reply.getAlbum();

        System.out.printf("%s[*] album files:\n", this.prompt);
        for (File file : album.getFilesList()) {
            System.out.printf(this.prompt + "[+] %s\n", file.getName());
        }

        System.out.printf(this.prompt + "[*] active users:\n");
        for (String user : album.getUsersList()) {
            System.out.printf(this.prompt + "[+] '%s' ", user);
        }
    }

    @Override
    public String usage() {
        return this.prompt + "[?] show : list files of the current album\n";
    }

    @Override
    public String help() {
        return "";
    }
}
