package org.cli.commands;

public interface Command {

    boolean matches(String command);
    void execute(String command, Object state);
}
