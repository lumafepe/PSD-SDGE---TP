package org.cli;

import org.cli.commands.Command;

import java.util.List;
import java.util.Scanner;

public class CLI {

    private Object state;
    private List<Command> commands;

    public CLI(List<Command> commands, Object initialState) {
        this.commands = commands;
        this.state = initialState;
    }

    public void run() {
        Scanner sc = new Scanner(System.in);

        System.out.print(this.startLine());

        while(sc.hasNextLine()) {
            String command = sc.nextLine();

            boolean success = false;
            for(Command cmd : commands) {
                if (cmd.matches(command)) {
                    cmd.execute(command, state);
                    success = true;
                    break;
                }
            }

            if(!success) {
                System.out.println("Unknown command");
            }

            System.out.print(this.startLine());
        }
    }

    private String startLine() {
        return "[home] ";
    }
}
