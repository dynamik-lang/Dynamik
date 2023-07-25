use clap::{Arg, ArgAction, Command, ArgMatches};

pub fn get_matches() -> ArgMatches {
    let cmd = Command::new("Dynamik Compiler")
        .about("")
        .version("1.0")
        .bin_name("dyc")
        .arg_required_else_help(true)
        .subcommand_required(true)
        .arg(
            Arg::new("opt level")
                .required(false)
                .long("opt_level")
                .action(ArgAction::Set)
                .default_value("0"),
        )
        .subcommand(
            Command::new("compile")
                .about("Compile Dynamik code")
                .arg_required_else_help(true)
                .arg(Arg::new("file path").action(ArgAction::Set).required(true))
        )
        .subcommand(
            Command::new("run")
                .about("Jit execute Dynamik code")
                .arg_required_else_help(true)
                .arg(Arg::new("file path").required(true).action(ArgAction::Set)),
        );

    cmd.get_matches()
}
