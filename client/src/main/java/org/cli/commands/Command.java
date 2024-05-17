package org.cli.commands;

public interface Command {

    boolean matches(String command);
    void execute(String command);
    String usage();
    String help();
}
