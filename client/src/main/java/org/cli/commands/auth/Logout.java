package org.cli.commands.auth;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Pattern;

public class Logout implements Command {

    public final String re = "logout|logout help";

    private final String prompt;

    public Logout(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("logout help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send("/logout");

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to logout: '%s'\n", reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] logged out\n");
    }

    @Override
    public String usage() {
        return this.prompt + "[?] logout : log off the current account\n";
    }

    @Override
    public String help() {
        return "";
    }
}

