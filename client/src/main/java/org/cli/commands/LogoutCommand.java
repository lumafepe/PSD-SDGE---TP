package org.cli.commands;

public class LogoutCommand implements Command {

    final String command = "logout";

    @Override
    public boolean matches(String input) {
        return input.trim().equalsIgnoreCase(command);
    }

    @Override
    public void execute(String input, Object state) {
        this.execute((Logout) null, state);
    }

    public void execute(Logout logout, Object state /* TODO: Type appropriately */) {
        // TODO: Implement the logic for logging out the user
    }

    private record Logout() {}
}
