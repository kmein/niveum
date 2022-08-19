project=Filli
year=2022

for month in Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec; do
  from="$(date +%F -d "$month 1, $year")"
  to="$(date +%F -d "$month 1, $year + 1 month")"
  watson report --json --from "$from" --to "$to" --project "$project"
done | jq --slurp '
  def in_array($arr):
      . as $value | any($arr[]; . == $value);

  map(
    ["engadin-app","fysiweb","val-muestair","mia-engiadina","ol"] as $official_projects
    | (.timespan.from | .[0:7]) as $timespan
    | .projects | .[0]
    | .time as $total_time
    | .tags
    | select(. != null)
    | map(select(.name | in_array($official_projects)))
    | (map(.time)|add) as $official_time
    | map({key:.name, value:.time}) | from_entries
    | .other |= ($total_time - $official_time)
    | map_values(. / (60*60) | ceil)
    | .month |= $timespan
  )
'
