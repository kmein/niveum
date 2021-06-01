#!/usr/bin/env node
const { createInterface } = require("readline");
const Sanscript = require("@sanskrit-coders/sanscript");

const rl = createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
});

rl.on("line", (line) => {
  console.log(Sanscript.t(line, "hk", "devanagari"));
});
