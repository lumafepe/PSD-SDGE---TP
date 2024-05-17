package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RemoveUser implements Command {

    public final String re = "remove (.*)|remove help";

    private final String prompt;

    public RemoveUser(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private RemoveUserCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new RemoveUserCommand(matcher.group(1));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("remove help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        RemoveUserCommand user = this.parseCommand(command);
        if (user == null) {
            System.out.printf(this.prompt + "[-] could not parse remove command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/removeUser %s", user.username));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to remove user '%s': '%s'\n", user.username, reply.getErrorMessage().getMessage());
            return;
        }
        System.out.printf(this.prompt + "[*] removed user '%s' from this session\n", user.username);
    }

    @Override
    public String usage() {
        return this.prompt + "[?] remove <username> : remove an user to the album session\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record RemoveUserCommand(String username) {}
}
