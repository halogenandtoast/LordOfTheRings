---
description: Pull the Ready column of the GitHub project board into parallel Herdr agents, one per issue, each investigating it
allowed-tools: Bash, Read, Skill
---

# Herd the board's Ready column into agents

Fan the open issues sitting in one column of [project 3](https://github.com/users/halogenandtoast/projects/3)
out into Herdr tabs, one Claude agent per issue in its own tab, each kicked off
with a prompt to investigate it. You are the dispatcher — you do **not**
investigate anything yourself.

`$ARGUMENTS` may name a column (default `Ready`), a cap on how many to spawn, or
an agent kind. Absent all three, take the Ready column in full with `claude`.

## 1. Confirm you are inside Herdr

```bash
test "${HERDR_ENV:-}" = 1
```

If it fails, say you are not running inside a Herdr pane and stop. Do not try to
drive the user's focused session from outside.

## 2. Find the cards

```bash
.claude/scripts/board-issues.sh [STATUS] [PROJECT_NUMBER]
```

Prints `number<TAB>url<TAB>title` for every **open** issue whose Status field on
project 3 matches `STATUS` (default `Ready`).

> **Do not reach for `gh project item-list`.** It pages through every Done card
> and truncates. Anything in an active column is an open issue, so the script
> walks the open issues instead and reads Status off each one's project item.

Open issues that were never added to the board have no Status and are never
listed. If the column is empty but `gh issue list` shows open issues, tell the
user they need to be put on the board.

If the column is empty, say so and stop.

## 3. Spawn

```bash
.claude/scripts/board-issues.sh Ready | .claude/scripts/herd-issues.sh [--kind KIND]
```

Add `--dry-run` first if you want to show the user the list before committing.
The script:

- skips any issue that already has a live `issue-<N>` agent, so re-running
  picks up only what is new;
- gives each issue its own tab in the current workspace, labelled `#<N>`, so
  every agent gets a full-size terminal however many are running and the
  caller's pane is never carved up;
- never moves the user's focus;
- names each agent `issue-<N>` and sends the `PROMPT` at the top of the script
  (this repo has no `/investigate-issue`; the prompt asks the agent to read the
  issue, find the root cause, and propose a fix in plan mode);
- retries `agent start` while the new tab's shell comes up — Herdr rejects a
  pane as `agent_pane_busy` until its prompt is live.

**Check with the user before spawning more than 6 at once** — each one is a full
Claude session.

## 4. Report

Give the user a table of tab label, agent name, issue link, and title, then
note:

- the prompt ends in plan mode, so each agent will stop and wait for plan
  approval rather than editing anything;
- `herdr agent list` shows lifecycle state, `herdr agent get issue-<N>` and
  `herdr agent read issue-<N> --source recent-unwrapped` read one of them,
  `herdr tab focus <tab-id>` switches to one.

Do not poll the agents in a loop; hand control back.

## Notes

- Parallel runs are expected — the prompt scopes scratch files to
  `/tmp/issue-<N>/`, so they will not clobber each other.
- To herd a different column, pass it: `/herd-issues "To triage"`. Status options
  are `To triage`, `Backlog`, `Ready`, `In progress`, `In review`, `Done`.
