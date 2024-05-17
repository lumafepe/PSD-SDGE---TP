package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Pattern;

public class LeaveAlbum implements Command {

    public final String re = "leave|leave help";

    private final String prompt;
    private final String ctx;

    public LeaveAlbum(String prompt, String ctx) {
        this.prompt = prompt.strip();
        this.ctx = ctx;
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("leave help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send("/leaveAlbum " + this.ctx);

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to leave album '%s': '%s'\n", this.ctx, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] abandoned '%s' editing session\n", this.ctx);

    }

    @Override
    public String usage() {
        return this.prompt + "[?] leave : leave the current album editing session\n";
    }

    @Override
    public String help() {
        return "";
    }
}
