package org.cli.commands.base;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Albums;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Pattern;

public class ListAlbum implements Command {

    public final String re = "list|list help";
    private final String prompt;

    public ListAlbum(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("list help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send("/listAlbums");

        if (!reply.getType().equals(Type.ALBUMS)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to get albums: '%s'\n", reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] available albums:\n", reply.getAlbums());

        Albums list = reply.getAlbums();
        for (String album : list.getNamesList()) {
            System.out.printf(this.prompt + "[*] %s\n", album);
        }
    }

    @Override
    public String usage() {
        return this.prompt + "[?] list : list available albums\n";
    }

    @Override
    public String help() {
        return "";
    }
}
