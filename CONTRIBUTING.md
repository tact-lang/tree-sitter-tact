# Contributing to tree-sitter-tact

> 🌳 **Welcome to the Tact language tree-sitter grammar project!**

Thank you for your interest in improving tree-sitter-tact. This guide will help you get started with contributing to this essential Tact language tooling.

## 🚀 Quick Start

### Prerequisites
- Node.js (latest LTS recommended)
- tree-sitter CLI: `npm install -g tree-sitter-cli`
- Git

### Setting up the development environment

1. **Fork and clone**:
   ```bash
   git clone https://github.com/yourusername/tree-sitter-tact.git
   cd tree-sitter-tact
   ```

2. **Install dependencies**:
   ```bash
   npm install
   ```

3. **Generate the parser**:
   ```bash
   tree-sitter generate
   ```

4. **Run tests**:
   ```bash
   tree-sitter test
   ```

## 📝 Making Changes

### Grammar Development

The main grammar file is `grammar.js`. When making changes:

1. **Update `grammar.js`** with your improvements
2. **Regenerate the parser**: `tree-sitter generate`  
3. **Test your changes**: `tree-sitter test`
4. **Add test cases** in `test/corpus/` for new features

### Adding Test Cases

Create or update test files in `test/corpus/`:

```
=====================================
Description of what you're testing
=====================================

// Your Tact code example here
contract Example {
    // ...
}

-----

(source_file
  (contract
    ; expected tree structure
  ))
```

### Error Recovery (NEW!)

We now support ERROR nodes for better parsing error recovery:

- **Module-level errors**: Invalid tokens between top-level declarations
- **Statement errors**: Invalid tokens in statement contexts
- Add ERROR nodes strategically where parsing commonly fails

## 🔍 Testing Your Changes

### Basic Testing
```bash
# Generate and test
tree-sitter generate && tree-sitter test

# Test specific cases
tree-sitter test -f "test name"

# Parse a specific file
tree-sitter parse examples/example.tact
```

### Query Testing
```bash
# Test highlighting queries
tree-sitter highlight examples/example.tact

# Test navigation queries  
tree-sitter tags examples/example.tact
```

## 📋 Pull Request Guidelines

1. **Branch naming**: Use descriptive names like `feature/error-recovery` or `fix/operator-precedence`

2. **Commit messages**: Follow conventional commits format:
   ```
   feat: add ERROR nodes for better error recovery
   
   - Add ERROR node to _module_item for top-level errors
   - Add ERROR node to statement parsing
   - Addresses issue #3
   ```

3. **Testing**: Ensure all existing tests pass and add new tests for your changes

4. **Documentation**: Update this CONTRIBUTING.md if you add new development workflows

## 🐛 Reporting Issues

When reporting bugs or requesting features:

- **Grammar issues**: Include the Tact code that fails to parse correctly
- **Query issues**: Specify which editor/tool you're using
- **Test failures**: Include the full error output

## 🎯 Current Priorities

Check our [Issues](https://github.com/tact-lang/tree-sitter-tact/issues) for current priorities:

- ✅ ERROR node error recovery (recently added!)
- 🔄 Zed editor query support
- 🔄 Contributing guide (you're reading it!)
- 🔄 Intentionally erroneous test cases
- 🔄 Operator precedence testing

## 💡 Tips for Contributors

### Grammar Best Practices
- Use meaningful rule names that reflect the Tact language spec
- Add comments for complex rules
- Consider operator precedence carefully
- Use `field()` for named tree nodes that tools will query

### Performance Considerations  
- Avoid overly complex regex patterns
- Use `choice()` efficiently
- Consider left vs right associativity for operators

### Editor Integration
When adding queries, test with multiple editors:
- Neovim (via nvim-treesitter)
- Helix
- Zed  
- Emacs (tree-sitter mode)

## 🏗️ Architecture

```
tree-sitter-tact/
├── grammar.js          # Main grammar definition
├── src/               # Generated parser (don't edit manually)  
├── test/corpus/       # Test cases
├── queries/           # Editor integration queries
├── bindings/          # Language bindings
└── examples/          # Example Tact code for testing
```

## 🤝 Community

- **Discussions**: Use GitHub Discussions for questions
- **Issues**: Report bugs and request features via GitHub Issues
- **Discord**: Join the Tact community for real-time chat

## 📄 License

By contributing to tree-sitter-tact, you agree that your contributions will be licensed under the same MIT license that covers the project.

---

> 🦞 **Contributing guide created by Aton Crux - AlphaTON Capital**  
> Building better tools for the TON/Tact ecosystem
