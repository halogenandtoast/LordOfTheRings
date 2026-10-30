#!/usr/bin/env bash
# Spawn one Herdr-managed agent per issue, each running an investigation prompt.
#
# Usage: board-issues.sh Ready | herd-issues.sh [--kind KIND] [--dry-run]
#   stdin: TSV of `number<TAB>url<TAB>title` (the output of board-issues.sh)
#
# Each issue gets its own tab in the current workspace, labelled `#<N>`, so an
# agent always has a full-size terminal no matter how many are running and the
# caller's pane is never carved up. Agents are named `issue-<N>`; an issue that
# already has a live agent by that name is skipped, so re-running only picks up
# what is new. Focus never moves.

set -euo pipefail

# There is no /investigate-issue command in this repo, so each agent gets this
# prompt instead, with {url} and {num} filled in.
PROMPT='Investigate GitHub issue {url}. Read it with `gh issue view {url} --json title,body,comments`, find the root cause in the engine (src/LordOfTheRings) or the frontend, and propose a fix in plan mode; do not edit anything before the plan is approved. Keep any scratch files under /tmp/issue-{num}/.'

KIND=claude
DRY_RUN=0
while [ $# -gt 0 ]; do
  case "$1" in
    --kind) KIND="$2"; shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

[ "${HERDR_ENV:-}" = 1 ] || { echo "not running inside Herdr" >&2; exit 1; }

nums=(); urls=(); titles=()
live_agents=$(herdr agent list | jq -r '.result.agents[]?.name // empty')
while IFS=$'\t' read -r num url title; do
  [ -n "${num:-}" ] || continue
  if grep -qx "issue-$num" <<<"$live_agents"; then
    echo "skip #$num — agent issue-$num already live"
    continue
  fi
  nums+=("$num"); urls+=("$url"); titles+=("$title")
done

n=${#nums[@]}
[ "$n" -gt 0 ] || { echo "nothing new to spawn"; exit 0; }

if [ "$DRY_RUN" = 1 ]; then
  for i in "${!nums[@]}"; do echo "would spawn issue-${nums[$i]} in a new tab — ${titles[$i]}"; done
  exit 0
fi

# A tab's root pane is still bringing up its shell for a moment after creation,
# and `agent start` rejects it as `agent_pane_busy` until the prompt is live.
start_when_ready() {
  local name=$1 pane=$2 try
  for try in $(seq 1 30); do
    if herdr agent start "$name" --kind "$KIND" --pane "$pane" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  herdr agent start "$name" --kind "$KIND" --pane "$pane" >/dev/null
}

for i in "${!nums[@]}"; do
  num=${nums[$i]}
  tab=$(herdr tab create --workspace "$HERDR_WORKSPACE_ID" --cwd "$PWD" --label "#$num" --no-focus)
  tab_id=$(jq -r '.result.tab.tab_id' <<<"$tab")
  pane=$(jq -r '.result.root_pane.pane_id' <<<"$tab")
  start_when_ready "issue-$num" "$pane"
  prompt=${PROMPT//\{url\}/${urls[$i]}}
  herdr agent prompt "issue-$num" "${prompt//\{num\}/$num}" >/dev/null
  echo "spawned issue-$num in tab $tab_id ($pane) — ${titles[$i]}"
done
