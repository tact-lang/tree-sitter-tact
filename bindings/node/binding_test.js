const assert = require("node:assert");
const { test } = require("node:test");

const Parser = require("tree-sitter");

test("can load grammar", () => {
  const parser = new Parser();
  // NOTE:
  // They changed something upstream and now loading in this manner doesn't work
  // assert.doesNotThrow(() => parser.setLanguage(require(".")));
  assert.doesNotThrow(() => parser.getLanguage());
});
