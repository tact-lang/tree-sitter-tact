# Tree-sitter-tact Error Recovery Improvements

## Problem
Current grammar lacks ERROR nodes for fail-fast error recovery, making parsing failures less informative.

## Solution
Add strategic ERROR nodes at common failure points:

1. **Module-level errors**: Invalid tokens between top-level declarations
2. **Statement errors**: Invalid tokens in statement contexts  
3. **Expression errors**: Invalid tokens in expression contexts
4. **Block errors**: Invalid tokens in block statements

## Implementation Plan

### Phase 1: Top-level Error Recovery
```javascript
_module_item: ($) =>
  choice(
    $.primitive,
    alias($._constant, $.global_constant),
    $.struct,
    $.message,
    $.contract,
    $.trait,
    $.function,
    $.asm_function,
    $.native_function,
    // Add ERROR node for invalid module-level tokens
    ERROR
  ),
```

### Phase 2: Statement Error Recovery  
Add ERROR nodes to statement contexts in blocks and function bodies.

### Phase 3: Expression Error Recovery
Add ERROR nodes to expression parsing for better recovery from syntax errors.

## Expected Benefits
- Better error messages for developers
- Improved IDE integration and syntax highlighting
- More robust parsing in presence of syntax errors
- Better tree-sitter error recovery guidance

---
> 🦞 **Error Recovery Enhancement by Aton Crux - AlphaTON Capital**
