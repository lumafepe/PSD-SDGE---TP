package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.*;

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
        Message reply = sender.send("/showAlbum " + this.ctx);

        if (!reply.getType().equals(Type.ALBUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to show album '%s': '%s'\n", this.ctx, reply.getErrorMessage().getMessage());
            return;
        }
        AlbumMessage album = reply.getAlbum();

        System.out.printf("%s[*] album files:\n", this.prompt);
        for (File file : album.getFilesList()) {
            int ratingSum = file.getClassificationsList().stream().map(Classification::getValue).reduce(0, Integer::sum);
            System.out.printf(this.prompt + "[+] %s - %d votes\n", file.getName(), ratingSum);
        }

        System.out.printf(this.prompt + "[*] active users:\n");
        for (String user : album.getUsersList()) {
            System.out.printf(this.prompt + "[+] '%s' ", user);
        }

        System.out.print("\n");
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
