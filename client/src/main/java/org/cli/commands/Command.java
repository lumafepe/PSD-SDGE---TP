package org.cli.commands;

public interface Command {

    public boolean matches(String command);
    public void execute(String command);
    public String usage();
    public String help();
}
