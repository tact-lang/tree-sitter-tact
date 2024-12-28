# Contributing

Thank you for your interest in contributing to the Tact tree-sitter grammar! We welcome contributions from everyone.

## Quick Links
- [Issues](https://github.com/tact-lang/tree-sitter-tact/issues)
- [Pull Requests](https://github.com/tact-lang/tree-sitter-tact/pulls)
- [Tree-sitter Documentation](https://tree-sitter.github.io/tree-sitter/)

## Getting Started

Before you begin:
- Check if there's already an [open issue](https://github.com/tact-lang/tree-sitter-tact/issues) addressing your problem
- Check out existing [pull requests](https://github.com/tact-lang/tree-sitter-tact/pulls) to see if someone else is working on similar changes

## How to Contribute

### Reporting Issues
- Include code snippets and error messages
- Include as much detail as possible
- Include steps to reproduce the issue
- Include the version of tree-sitter you're using
- Use code blocks for error messages and logs

### Making Changes
1. Fork the repository
2. Create a new branch for your changes
3. Make your changes
4. Write or update tests as needed
5. Ensure all tests pass
6. Format the code using `npm run fmt`
7. Submit a pull request

### Pull Request Process
1. Update the README.md if needed
2. Follow the existing code style
3. Include a clear description of your changes
4. Link any related issues using keywords (e.g., "Fixes #123")
5. Wait for review from maintainers

## Development Setup

This repository contains the Tact language grammar for the [Tree-Sitter](https://tree-sitter.github.io/tree-sitter/)
parsing library.

### Building grammar

To generate the grammar after making changes, you first need to install tree-sitter-cli if it is not already
installed:

```
npm install
```

Now to build the grammar, use the command:

```
npm run build
```

To generate wasm, use the command:

```
npm run build-wasm
```

### Testing grammar

Grammar tests are divided into several types and placed in their own folders inside `test/`:

- AST tests (`test/corpus`):

  Check that the generated AST matches the expected one.

- Highlighting tests (`test/highlight`):

  Check that parts of the code have certain type kinds (`keyword`, `type.builtin`, `punctuation.delimiter`, etc.).

- Tags tests (`test/tags`):

  Check that parts of the code have certain tags (`definition.method`, `definition.constant`, etc.).

After making changes to the grammar, run the command:

```
tree-sitter generate && tree-sitter test
```

to test your changes.

If you see differences after the changes, and you are **absolutely sure** that they are correct (for example, if you
changed a rule name), then add the `-u` flag to automatically update the tests:

```
tree-sitter test -u
```

For more details, see the official testing documentation in
the [tree-sitter documentation](https://tree-sitter.github.io/tree-sitter/creating-parsers/5-writing-tests.html).

### Changing grammar

When changing rules in the grammar, you need a way to check if the file is parsed correctly. There are two options
for checking this.

Use the command:

```
tree-sitter parse file.tact
```

to get an AST. If there are errors in it, the output will contain ERROR or MISSING nodes.

To find out why parsing was unsuccessful, add the `-D --open-log` flags to open the parsing step-by-step report in the
browser.

The second method involves using the web playground via the command:

```
npm run play
```

This playground is convenient for changing code in real time and looking at the resulting AST. Don't forget to restart
it after changing the grammar!

After changing the grammar, don't forget to format it using the command:

```
npm run fmt
```

#### Queries

When adding new rules or renaming existing ones, remember to add/rename them in queries located in the `queries/`
and `editor_queries/` folders!
