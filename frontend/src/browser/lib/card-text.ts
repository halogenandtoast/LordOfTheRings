/**
 * Card text parsing.
 *
 * Two dialects arrive in the same field. RingsDB writes player-card text with
 * light HTML (`<b>`, `<i>`, `<cite>`) and `[sphere]` icon tokens; OCTGN writes
 * encounter-card text as plain prose. Both are parsed into the same token
 * stream so one component renders either, and so nothing is passed to
 * `v-html`.
 */

export type TextToken =
  | { kind: 'text'; value: string }
  | { kind: 'break' }
  | { kind: 'icon'; name: IconName }
  | { kind: 'keyword'; value: string }
  | { kind: 'emphasis'; style: 'bold' | 'italic' | 'cite'; children: TextToken[] }

export const ICON_NAMES = [
  'leadership',
  'lore',
  'spirit',
  'tactics',
  'baggins',
  'fellowship',
  'neutral',
  'willpower',
  'attack',
  'defense',
  'threat',
  'per-player',
] as const

export type IconName = (typeof ICON_NAMES)[number]

const ICON_SET = new Set<string>(ICON_NAMES)

/**
 * Ability keywords that open a line of card text. RingsDB bolds them already;
 * OCTGN does not, so they are recognised and marked up here to keep encounter
 * cards as readable as player cards.
 */
const ABILITY_KEYWORDS = [
  'When Revealed',
  'Forced',
  'Response',
  'Action',
  'Quest Action',
  'Planning Action',
  'Combat Action',
  'Refresh Action',
  'Resource Action',
  'Encounter Action',
  'Travel Action',
  'Valour Action',
  'Valour Response',
  'Time Action',
  'Setup',
  'Travel',
  'Shadow',
  'Condition',
  'Escape',
  'Resolution',
  'Objective',
]

// Longest first, so "Valour Response" wins over "Response".
const KEYWORD_PATTERN = new RegExp(
  `^(${[...ABILITY_KEYWORDS].sort((a, b) => b.length - a.length).join('|')}):`,
)

const TAG_PATTERN = /<(\/?)(b|i|cite)>/gi
const ICON_PATTERN = /\[([a-z-]+)\]/gi

/** Split a run of plain text on `[icon]` tokens and newlines. */
function tokenizeRun(run: string, markKeywords: boolean): TextToken[] {
  const tokens: TextToken[] = []
  let atLineStart = markKeywords

  for (const [lineIndex, line] of run.split('\n').entries()) {
    if (lineIndex > 0) {
      tokens.push({ kind: 'break' })
      atLineStart = markKeywords
    }

    let rest = line
    if (atLineStart) {
      const keyword = KEYWORD_PATTERN.exec(rest)
      if (keyword?.[1]) {
        tokens.push({ kind: 'keyword', value: `${keyword[1]}:` })
        rest = rest.slice(keyword[0].length)
      }
      atLineStart = false
    }

    ICON_PATTERN.lastIndex = 0
    let cursor = 0
    let match: RegExpExecArray | null
    while ((match = ICON_PATTERN.exec(rest)) !== null) {
      const name = match[1]?.toLowerCase()
      if (!name || !ICON_SET.has(name)) continue
      if (match.index > cursor) {
        tokens.push({ kind: 'text', value: rest.slice(cursor, match.index) })
      }
      tokens.push({ kind: 'icon', name: name as IconName })
      cursor = match.index + match[0].length
    }
    if (cursor < rest.length) tokens.push({ kind: 'text', value: rest.slice(cursor) })
  }

  return tokens
}

/**
 * Parse card text into a token tree.
 *
 * Unclosed and mismatched tags are tolerated — one card in the set ships a
 * literal `<b.Response:</b>` — and a stray `<` is treated as text.
 */
export function parseCardText(input: string | null | undefined): TextToken[] {
  if (!input) return []

  // Only auto-bold ability keywords when the source did not mark them itself.
  const markKeywords = !/<b>/i.test(input)

  const root: TextToken[] = []
  const stack: TextToken[][] = [root]
  const openTags: string[] = []

  const push = (tokens: TextToken[]) => {
    const top = stack[stack.length - 1]
    if (top) top.push(...tokens)
  }

  TAG_PATTERN.lastIndex = 0
  let cursor = 0
  let match: RegExpExecArray | null

  while ((match = TAG_PATTERN.exec(input)) !== null) {
    if (match.index > cursor) {
      push(tokenizeRun(input.slice(cursor, match.index), markKeywords))
    }
    cursor = match.index + match[0].length

    const closing = match[1] === '/'
    const tag = (match[2] ?? '').toLowerCase()

    if (closing) {
      // Ignore a close with no matching open rather than unwinding the stack.
      const depth = openTags.lastIndexOf(tag)
      if (depth === -1) continue
      while (openTags.length > depth) {
        openTags.pop()
        stack.pop()
      }
    } else {
      const node: TextToken = {
        kind: 'emphasis',
        style: tag === 'b' ? 'bold' : tag === 'i' ? 'italic' : 'cite',
        children: [],
      }
      push([node])
      openTags.push(tag)
      stack.push(node.children)
    }
  }

  if (cursor < input.length) push(tokenizeRun(input.slice(cursor), markKeywords))
  return root
}

/** Flatten to plain text, for tooltips and copy-to-clipboard. */
export function tokensToPlainText(tokens: TextToken[]): string {
  return tokens
    .map((token) => {
      switch (token.kind) {
        case 'text':
          return token.value
        case 'keyword':
          return token.value
        case 'break':
          return '\n'
        case 'icon':
          return `[${token.name}]`
        case 'emphasis':
          return tokensToPlainText(token.children)
      }
    })
    .join('')
}
