import { cardImage, type CardView, type GameView } from '../types'

const DEAL_MS = 520

/**
 * Cards moving between a player's hand or play area and their deck or discard have nothing
 * on screen at one end of the move, so for the view transition a stand-in is laid on the
 * pile under the card's transition name: a card back on the deck, which turns over on the
 * way, or the card's face on the discard.
 *
 * Call `dealFrom` before the transition starts. As the new state goes in, `lift` the
 * stand-ins the cards leave from, then `land` (once the new board is rendered) the ones
 * cards arrive at, and call `done` when the transition has finished.
 */
export const dealFrom = (before: GameView | null, after: GameView) => {
  if (!before?.started) return null
  const leaving: HTMLElement[] = []
  const arriving: HTMLElement[] = []
  const rules: string[] = []
  const onScreen = (id: string) => document.querySelector(`[data-card-id="${id}"]`) !== null

  const stand = (pile: string, card: CardView, faceDown: boolean) => {
    const ghost = faceDown ? document.createElement('div') : Object.assign(document.createElement('img'), { src: cardImage(card), alt: '' })
    if (faceDown) ghost.className = 'pile player'
    else ghost.style.borderRadius = '7px'
    ghost.dataset.pile = pile
    ghost.style.setProperty('view-transition-name', `card-${card.id}`)
    rules.push(`::view-transition-group(card-${card.id}) { animation-duration: ${DEAL_MS}ms; z-index: 1; }`)
    if (faceDown)
      rules.push(
        `::view-transition-image-pair(card-${card.id}) { perspective: 800px; }`,
        `::view-transition-old(card-${card.id}) { animation: deal-turn-away ${DEAL_MS}ms ease-in both; }`,
        `::view-transition-new(card-${card.id}) { animation: deal-turn-up ${DEAL_MS}ms ease-out both; }`,
      )
    return ghost
  }

  const lay = (ghost: HTMLElement) => {
    const pile = ghost.dataset.pile?.startsWith('deck')
      ? document.querySelector(`[data-pile="${ghost.dataset.pile}"] .pile`)
      : document.querySelector(`[data-pile="${ghost.dataset.pile}"] .discard`)
    if (!pile) return
    const { left, top, width, height } = pile.getBoundingClientRect()
    Object.assign(ghost.style, { position: 'fixed', left: `${left}px`, top: `${top}px`, width: `${width}px`, height: `${height}px`, margin: '0' })
    ghost.removeAttribute('data-pile') // so pile lookups only find the real piles
    document.body.append(ghost)
  }

  for (const player of after.players) {
    const was = before.players.find((p) => p.playerId === player.playerId)
    if (!was) continue
    const deck = `deck-${player.playerId}`
    const discard = `discard-${player.playerId}`
    const inDiscard = (view: typeof player) => new Set(view.discard.map((card) => card.id))
    const wasDiscarded = inDiscard(was)
    const isDiscarded = inDiscard(player)

    // arrivals: anything new to the hand or play area came from the discard or else the deck
    for (const card of [...player.hand, ...player.inPlay]) {
      if (onScreen(card.id)) continue // the top of the discard, say: it morphs on its own
      leaving.push(stand(wasDiscarded.has(card.id) ? discard : deck, card, !wasDiscarded.has(card.id)))
    }

    // departures: hand and play cards that went into the discard, or back into a growing deck
    const kept = new Set([...player.hand, ...player.inPlay].map((card) => card.id))
    for (const card of [...was.hand, ...was.inPlay]) {
      if (kept.has(card.id)) continue
      if (isDiscarded.has(card.id)) arriving.push(stand(discard, card, false))
      else if (!isDiscarded.has(card.id) && player.deckSize > was.deckSize) arriving.push(stand(deck, card, true))
    }
  }

  leaving.forEach(lay)
  if (!leaving.length && !arriving.length) return null
  const style = Object.assign(document.createElement('style'), { textContent: rules.join('\n') })
  document.head.append(style)
  return {
    lift: () => leaving.forEach((ghost) => ghost.remove()),
    // ponytail: a card that left for somewhere else drawn on screen (an attachment, say) is skipped here
    land: () => arriving.filter((ghost) => !onScreen(ghost.style.getPropertyValue('view-transition-name').slice(5))).forEach(lay),
    done: () => {
      arriving.forEach((ghost) => ghost.remove())
      style.remove()
    },
  }
}
