# Clap: Command-Line Argument Parsing

Clap is a library for command-line argument parsing.
Clap works by directly consuming arguments in an imperative way.
Traditionally, argument parsing in OCaml is done by first defining
a specification (an OCaml value defining the types of arguments),
and then parsing from this specification.
The "impure" approach of Clap skips the need to define a specification
and results in code which is quite simple in practice,
with limited boilerplate.

Clap does construct a specification internally as arguments are parsed.
It uses it to generate the `--help`, following usual man page conventions.

## API Documentation

See [src/clap.mli](src/clap.mli).

## Example

See [demo/demo.ml](demo/demo.ml).

## The Two Rules

One limitation of Clap is that you have to follow two rules which are not enforced
by typing:
- read named arguments before unnamed arguments;
- call `Clap.close` before actually using arguments.

There are exceptions to these rules.

- You can read a named argument after an unnamed argument if you are ok that
  the named argument cannot appear before the unnamed argument.
  For instance, if the unnamed argument is a subcommand name and the named argument
  only exists for this subcommand, you can read the command name first and then
  decide whether or not to read the named argument. Users will not be able to
  specify the named argument before the command name but this behavior is usually ok
  for subcommands.

- If an argument cannot be invalid, i.e. it is not mandatory and all strings
  are valid values for it, then you can use its value before calling `Clap.close`.
