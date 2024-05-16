package org.cli.commands.album;

import org.cli.Sender;
import org.cli.commands.Command;
import org.messages.central.Message;
import org.messages.central.Type;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Chat implements Command {

    public final String re = "chat (.*)|chat help";

    private final String prompt;

    public Chat(String prompt) {
        this.prompt = prompt.strip();
    }

    @Override
    public boolean matches(String command) {
        return Pattern.matches(this.re, command);
    }

    private ChatCommand parseCommand(String command) {
        Matcher matcher = Pattern.compile(this.re).matcher(command);
        if (matcher.find()) {
            return new ChatCommand(matcher.group(1));
        }
        return null;
    }

    @Override
    public void execute(String command) {

        if (command.equalsIgnoreCase("chat help")) {
            System.out.print(this.usage());
            System.out.print(this.help());
            return;
        }

        ChatCommand message = this.parseCommand(command);
        if (message == null) {
            System.out.printf(this.prompt + "[-] could not parse chat command from '%s'\n", command);
            System.out.print(this.usage());
            return;
        }

        Sender sender = Sender.getInstance();
        Message reply = sender.send(String.format("/chat %s", message.content));

        if (!reply.getType().equals(Type.SUCESIUM)) {
            System.out.printf(this.prompt + "[-] an error occurred while trying to use chat: '%s'\n", reply.getErrorMessage().getMessage());
        }
    }

    @Override
    public String usage() {
        return this.prompt + "[?] chat <message> : send a message to the current album session\n";
    }

    @Override
    public String help() {
        return "";
    }

    private record ChatCommand(String content) {}
}
