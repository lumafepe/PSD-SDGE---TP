package org.cli.commands.auth;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Register implements Command {

    public final String re = "register (\\S+) (\\S+)|register help";
    private final String prompt;

    public Register(String prompt) {
        this.prompt = prompt.strip();
    }

    private RegisterCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new RegisterCommand(matcher.group(1), matcher.group(2));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("register help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        RegisterCommand credentials = this.parseCommand(command);
        if (credentials == null) {
            System.out.printf("[-] could not parse register credentials from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/register %s %s", credentials.username, credentials.password));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to register: '%s'\n", reply.getErrorMessage().getMessage());
        }

        System.out.printf(this.prompt + "[*] user '%s' registered with success\n", credentials.username);
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] register <username> <password> : register a new user into the application\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record RegisterCommand(String username, String password) {}
}
