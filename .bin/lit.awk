BEGIN {
  if (!comment) comment = "--";
  if (!begin) begin = "\\begin{code}";
  if (!end) end = "\\end{code}";
}
{
  if ($0 == begin) {
    code = 1;
    print comment, $0;
  } else if ($0 == end) {
    code = 0;
    print comment, $0;
  } else {
    if (code) print $0;
    else print comment, $0;
  }
}
