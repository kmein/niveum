{
  coreutils,
  git,
  writers,
}:
writers.writeDashBin "git-preview" ''
  set -efu
  head_commit=$(${git}/bin/git log -1 --format=%H)
  merge_commit=$1; shift
  merge_message='Merge for git-preview'
  preview_dir=$(${coreutils}/bin/mktemp --tmpdir -d git-preview.XXXXXXXX)
  preview_name=$(${coreutils}/bin/basename "$preview_dir")
  ${git}/bin/git worktree add --detach -f "$preview_dir" 2>/dev/null
  ${git}/bin/git -C "$preview_dir" checkout -q "$head_commit"
  ${git}/bin/git -C "$preview_dir" merge \
      ''${GIT_PREVIEW_MERGE_STRATEGY+-s "$GIT_PREVIEW_MERGE_STRATEGY"} \
      -m "$merge_message" \
      -q \
      "$merge_commit"
  ${git}/bin/git -C "$preview_dir" diff "$head_commit.." "$@"
  ${coreutils}/bin/rm -fR "$preview_dir"
  ${coreutils}/bin/rm -R .git/worktrees/"$preview_name"
''
