package org.cli;

import org.cli.commands.Command;
import org.cli.commands.auth.Login;
import org.cli.commands.auth.Register;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Root implements Command {

    private static final String prompt = "[home] ";

    private final List<Command> subcommands = Arrays.asList(
            new Login(Root.prompt),
            new Register(Root.prompt)
    );

    @Override
    public void execute(String command) {

        Scanner sc = new Scanner(System.in);

        System.out.print(Root.prompt);
        while (sc.hasNextLine()) {

            boolean success = false;
            String c = sc.nextLine();

            if (c.equalsIgnoreCase("help")) {
                System.out.println(this.help());
                System.out.print(Root.prompt);
                continue;
            }

            for (Command cmd : this.subcommands) {
                if (cmd.matches(c)) {
                    cmd.execute(c);
                    success = true;
                    break;
                }
            }

            if (!success) {
                System.out.printf(Root.prompt.strip() + "[-] unknown command '%s'%n", c);
            }

            System.out.print(Root.prompt);
        }
    }

    @Override
    public boolean matches(String command) {
        return false;
    }

    @Override
    public String usage() {
        return "";
    }

    @Override
    public String help() {
        return Root.prompt.strip() + "[?] available commands: login, register";
    }

}
