package org.cli.commands;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ChatBroadcastCommand implements Command {

    final String regex = "chat broadcast (.+)";

    @Override
    public boolean matches(String command) {
        return Pattern.matches(regex, command);
    }

    @Override
    public void execute(String command, Object state) {
        this.execute(this.parseCommand(command), state);
    }

    public void execute(ChatBroadcast command, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for broadcasting the chat message
    }

    private ChatBroadcast parseCommand(String command) {
        Matcher matcher = Pattern.compile(regex).matcher(command);
        if (matcher.find()) {
            return new ChatBroadcast(matcher.group(1));
        } else {
            throw new RuntimeException("Unexpected parsing of invalid command");
        }
    }

    private record ChatBroadcast(String message) {}
}
