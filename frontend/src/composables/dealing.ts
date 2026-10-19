import { cardImage, type CardView, type GameView } from '../types'

const DEAL_MS = 520
const STAGGER_MS = 90
const SHUFFLE_MS = 450

interface Move {
  /** The transition name it moves under. */
  name: string
  card: CardView
  pile: string
  faceDown: boolean
  ghost: HTMLElement
}

// cards that come from the encounter deck rather than a player's
const encounterTypes = new Set(['Enemy', 'Location', 'Treachery', 'Objective', 'Objective Ally'])

/**
 * Cards moving between a player's hand or play area and their deck or discard have nothing
 * on screen at one end of the move, so for the view transition a stand-in is laid on the
 * pile under the card's transition name: a card back on the deck, which turns over on the
 * way, or the card's face on the discard. Cards go back first, one after another; if any
 * went into a deck it shuffles, and then cards come out, one after another.
 *
 * Cards the engine says were shuffled back (`shuffledBack`) go back even if they are drawn
 * straight out again.
 *
 * Call `dealFrom` before the transition starts. As the new state goes in, `lift` the
 * stand-ins the cards leave from, then `land` (once the new board is rendered) the ones
 * cards arrive at, and call `done` when the transition has finished.
 */
export const dealFrom = (before: GameView | null, after: GameView, shuffledBack: ReadonlySet<string> = new Set()) => {
  if (!before?.started) return null
  const leaving: Move[] = []
  const arriving: Move[] = []
  const renamed: HTMLElement[] = []
  const shuffling: string[] = []
  const onScreen = (id: string) => document.querySelector(`[data-card-id="${id}"]`) !== null

  const move = (pile: string, card: CardView, faceDown: boolean, name = `card-${card.id}`): Move => {
    const ghost = faceDown ? document.createElement('div') : Object.assign(document.createElement('img'), { src: cardImage(card), alt: '' })
    if (faceDown) ghost.className = `pile ${pile.endsWith('-encounter') ? 'encounter' : 'player'}`
    else ghost.style.borderRadius = '7px'
    ghost.style.setProperty('view-transition-name', name)
    return { name, card, pile, faceDown, ghost }
  }

  const pileAt = (pile: string) =>
    document.querySelector<HTMLElement>(`[data-pile="${pile}"] ${pile.startsWith('deck') ? '.pile' : '.discard'}`)

  const lay = ({ pile, ghost }: Move) => {
    const at = pileAt(pile)
    if (!at) return
    const { left, top, width, height } = at.getBoundingClientRect()
    Object.assign(ghost.style, { position: 'fixed', left: `${left}px`, top: `${top}px`, width: `${width}px`, height: `${height}px`, margin: '0' })
    document.body.append(ghost)
  }

  // every card id the new board shows anywhere
  const onBoard = new Set(JSON.stringify(after).match(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g))

  for (const player of after.players) {
    const was = before.players.find((p) => p.playerId === player.playerId)
    if (!was) continue
    const deck = `deck-${player.playerId}`
    const discard = `discard-${player.playerId}`
    const wasDiscarded = new Set(was.discard.map((card) => card.id))
    const isDiscarded = new Set(player.discard.map((card) => card.id))

    // departures: hand and play cards that went into the discard, or left the board for the deck
    // ponytail: anything else that leaves the board (removed from the game, say) is taken for the deck too
    for (const card of [...was.hand, ...was.inPlay]) {
      if (shuffledBack.has(card.id) && onBoard.has(card.id)) {
        // shuffled back and drawn straight out again: one card cannot make both trips under
        // one name, so the trip in goes under another, the card taking it until `lift`
        const shown = document.querySelector<HTMLElement>(`[data-card-id="${card.id}"]`)
        shown?.style.setProperty('view-transition-name', `back-${card.id}`)
        if (shown) renamed.push(shown)
        arriving.push(move(deck, card, true, `back-${card.id}`))
      } else if (isDiscarded.has(card.id)) {
        if (!wasDiscarded.has(card.id)) arriving.push(move(discard, card, false))
      } else if (!onBoard.has(card.id)) arriving.push(move(deck, card, true))
    }
    if (arriving.some((m) => m.pile === deck)) shuffling.push(player.playerId)

    // arrivals: anything new to the hand or play area came from the discard or else the deck
    // (encounter cards attached in play came from the encounter deck, below)
    for (const card of [...player.hand, ...player.inPlay.filter((c) => !encounterTypes.has(c.type))]) {
      // the top of the discard, say, morphs on its own
      if (onScreen(card.id) && !shuffledBack.has(card.id)) continue
      leaving.push(wasDiscarded.has(card.id) ? move(discard, card, false) : move(deck, card, true))
    }
  }

  // the encounter deck and discard: cards revealed or searched out of them, and cards
  // defeated or discarded into the discard
  if (before.scenario && after.scenario) {
    const wasDiscarded = new Set(before.scenario.discard.map((card) => card.id))
    const isDiscarded = new Set(after.scenario.discard.map((card) => card.id))
    const inPlay = (view: GameView) =>
      [
        ...(view.scenario?.staging ?? []),
        ...(view.scenario?.activeLocation ? [view.scenario.activeLocation] : []),
        ...view.players.flatMap((p) => [...p.engaged, ...p.inPlay]),
      ].filter((card) => encounterTypes.has(card.type))
    for (const card of inPlay(before))
      if (isDiscarded.has(card.id) && !wasDiscarded.has(card.id)) arriving.push(move('discard-encounter', card, false))
    for (const card of inPlay(after)) {
      if (onScreen(card.id)) continue
      leaving.push(wasDiscarded.has(card.id) ? move('discard-encounter', card, false) : move('deck-encounter', card, true))
    }
  }
  if (!leaving.length && !arriving.length) return null

  // the timeline: cards going back, then any shuffle, then cards coming out
  const rules: string[] = []
  const animate = ({ name, faceDown }: Move, delay: number, vanish: boolean) => {
    rules.push(`::view-transition-group(${name}) { animation-duration: ${DEAL_MS}ms; animation-delay: ${delay}ms; animation-fill-mode: both; z-index: 1; }`)
    // a card gone into the deck disappears into it, so the deck can shuffle
    const gone = vanish ? `, deal-vanish 1ms ${delay + DEAL_MS}ms forwards` : ''
    if (faceDown)
      rules.push(
        `::view-transition-image-pair(${name}) { perspective: 800px; }`,
        `::view-transition-old(${name}) { animation: deal-turn-away ${DEAL_MS}ms ease-in ${delay}ms both; }`,
        `::view-transition-new(${name}) { animation: deal-turn-up ${DEAL_MS}ms ease-out ${delay}ms both${gone}; }`,
      )
    else rules.push(`::view-transition-old(${name}), ::view-transition-new(${name}) { animation-delay: ${delay}ms; animation-fill-mode: both; }`)
  }
  arriving.forEach((m, i) => animate(m, i * STAGGER_MS, m.faceDown))
  const shuffleAt = arriving.length ? (arriving.length - 1) * STAGGER_MS + DEAL_MS : 0
  const dealAt = shuffling.length ? shuffleAt + SHUFFLE_MS : 0
  leaving.forEach((m, i) => animate(m, dealAt + i * STAGGER_MS, false))

  // the deck takes its own transition name while it shuffles, so it can move on its own
  const decks = shuffling.flatMap((pid) => pileAt(`deck-${pid}`) ?? [])
  decks.forEach((deck, i) => {
    const name = `deck-${shuffling[i]}`
    deck.style.setProperty('view-transition-name', name)
    rules.push(
      `::view-transition-old(${name}) { animation: none; opacity: 0; }`,
      `::view-transition-new(${name}) { animation: deck-shuffle ${SHUFFLE_MS}ms ease-in-out ${shuffleAt}ms both; }`,
    )
  })

  leaving.forEach(lay)
  const style = Object.assign(document.createElement('style'), { textContent: rules.join('\n') })
  document.head.append(style)
  return {
    lift: () => {
      leaving.forEach((m) => m.ghost.remove())
      renamed.forEach((shown) => shown.style.setProperty('view-transition-name', `card-${shown.dataset.cardId}`))
    },
    // ponytail: a card that left for somewhere else drawn on screen (an attachment, say) is skipped here
    land: () => arriving.filter((m) => m.name.startsWith('back-') || !onScreen(m.card.id)).forEach(lay),
    done: () => {
      arriving.forEach((m) => m.ghost.remove())
      decks.forEach((deck) => deck.style.removeProperty('view-transition-name'))
      style.remove()
    },
  }
}

/**
 * The board just before the opening hands were dealt: hands empty, their cards still in the
 * decks. Shown first when a game opens on the mulligan, so the opening hand deals out.
 */
export const beforeTheDeal = (view: GameView): GameView | null =>
  view.question?.prompt === 'Keep this starting hand?'
    ? { ...view, players: view.players.map((p) => ({ ...p, hand: [], handSize: 0, deckSize: p.deckSize + p.hand.length })) }
    : null
