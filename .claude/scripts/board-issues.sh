#!/usr/bin/env bash
# List open issues sitting in a given column of the GitHub project board.
#
# Usage: board-issues.sh [STATUS] [PROJECT_NUMBER]
#   STATUS          Status field value to match (default: Ready)
#   PROJECT_NUMBER  User project number (default: 3)
#
# Output: TSV of `number<TAB>url<TAB>title`, one per line.
#
# Why not `gh project item-list`: it pages through every Done card on the board
# and truncates. Every card in an active column is an OPEN issue, so we walk the
# open issues instead and read Status off each one's project item.

set -euo pipefail

STATUS="${1:-Ready}"
PROJECT="${2:-3}"
OWNER=halogenandtoast
REPO=LordOfTheRings

cursor=null
while :; do
  resp=$(gh api graphql -F cursor="$cursor" -f query='
    query($cursor: String) {
      repository(owner: "'"$OWNER"'", name: "'"$REPO"'") {
        issues(first: 100, states: OPEN, after: $cursor, orderBy: {field: CREATED_AT, direction: DESC}) {
          pageInfo { hasNextPage endCursor }
          nodes {
            number title url
            projectItems(first: 5) {
              nodes {
                project { number }
                fieldValueByName(name: "Status") {
                  ... on ProjectV2ItemFieldSingleSelectValue { name }
                }
              }
            }
          }
        }
      }
    }')

  echo "$resp" | jq -r --argjson project "$PROJECT" --arg status "$STATUS" '
    .data.repository.issues.nodes[]
    | . as $i
    | $i.projectItems.nodes[]
    | select(.project.number == $project and .fieldValueByName.name == $status)
    | [$i.number, $i.url, $i.title] | @tsv'

  has_next=$(echo "$resp" | jq -r '.data.repository.issues.pageInfo.hasNextPage')
  [ "$has_next" = "true" ] || break
  cursor=$(echo "$resp" | jq -r '.data.repository.issues.pageInfo.endCursor')
done
