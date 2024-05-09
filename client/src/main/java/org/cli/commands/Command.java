package org.cli.commands;

public interface Command {

    public void execute(String command);

    public String description();
    public String usage();
}
