# Environment

This is NixOS. Any program is available on demand — do not conclude a tool is
missing because `command -v` finds nothing, and do not work around its absence.

    nix shell nixpkgs#<pkg> --command <cmd> [args]     # one-off
    nix shell nixpkgs#gh --command gh pr list          # e.g. gh, jq, ripgrep, httpie
    nix run nixpkgs#<pkg> -- [args]                    # when the binary shares the package name

Already in `PATH` and needing no wrapper: `git`, `python3`, `deno`, `chromium`,
`firefox`, `curl`, `nix`. Prefer those directly.

# Nix flakes

Write flakes in the schema `nix flake init` produces: `outputs = inputs:`,
each output a `builtins.mapAttrs (system: pkgs: …)` over
`inputs.nixpkgs.legacyPackages`. No flake-utils or flake-parts. Run
`nix flake init` in a scratch directory to see the current template, and take
the nixpkgs input from it too.

# Recovering a lost version of a file

I keep nvim persistent undo, so a file overwritten on disk is usually still
recoverable — a buffer reload is itself an undo step. Try this before saying
work is lost.

Undofiles live in `~/.vim/undo/`, one per buffer, the absolute path escaped
as `%home%kfm%…%thesis.typ`. Work read-only on a copy; never open the real file.

    U='/home/kfm/.vim/undo/\%home\%kfm\%…\%thesis.typ'   # escape every % or :rundo expands it to the buffer name
    cp <the file as it is now> /tmp/rec/f.typ            # :rundo checks a hash, so start from what is on disk
    nvim --headless -u NONE -i NONE -n /tmp/rec/f.typ \
      -c "rundo $U" -c "call writefile([json_encode(undotree())],'/tmp/tree.json')" -c qa!
    # find the seq by its timestamp in tree.json, then extract that state
    nvim --headless -u NONE -i NONE -n /tmp/rec/f.typ \
      -c "rundo $U" -c "undo <seq>" -c "write! /tmp/recovered" -c qa!

Gotchas: `:rundo` aborts with "File contents changed" unless the buffer hashes
to the value in the undofile (sha256 over each line plus a NUL), so the copy
must be byte-identical to what nvim last saw. Classic `vim` reports E824 on
these — the format is version 3, which is nvim's; use `nvim`. Diff the
extracted state against your own to tell my edits from yours.

Better still, do not need it: I edit files in the checkout while you work, so
never copy a file aside and later copy it back. Transform the current file
forward instead.

# Comments and code

Comments: as short as possible, lowercase, and about the *why*. The code
already says how. Drop a comment rather than restate the line below it.

Code, comments, identifiers and commit messages in English, even when we are
talking in another language.

# Commits

Commit straight to the branch that is checked out, `master` included. Do not
create a branch first.

## Commit messages

A subject in lower case and the imperative, prefixed with the component it
changes: `miniflux: pass a get-only client instead of globals`. A change to
several components joins them: `miniflux, wallabag: …`. Add a body only when
strictly necessary, as simple bullet points. No `Co-Authored-By` or other
attribution trailer.

# Documentation

Write down only what stays true. If a command already answers the question —
what exists, what builds, what is ignored — point at the command instead of
copying its output into prose I then have to keep in sync. No inventories, no
tables of current state, no counts or sizes, no examples pinned to whichever
item happened to be newest.

What earns a place: conventions, the non-obvious constraint, the reason a thing
is built the way it is, and facts nothing in the repo can tell you.
