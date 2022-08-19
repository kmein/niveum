#!/usr/bin/env node
const { createInterface } = require("readline");
const Sanscript = require("@indic-transliteration/sanscript");

const rl = createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
});

rl.on("line", (line) => {
  console.log(
    Sanscript.t(line, "hk", "devanagari")
      .replace(/\.\./g, "॥")
      .replace(/[,.]/g, "।")
  );
});
