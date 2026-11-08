/** Keep surviving cards in place; newly drawn or returned cards go on the right. */
export const reconcileHandOrder = (order: readonly string[], hand: readonly string[]): string[] => {
  const present = new Set(hand)
  const kept = [...new Set(order)].filter(id => present.has(id))
  const known = new Set(kept)
  return [...kept, ...hand.filter(id => !known.has(id))]
}

export const moveHandCard = (order: readonly string[], card: string, before: string | null): string[] => {
  if (!order.includes(card) || card === before || (before !== null && !order.includes(before))) return [...order]
  const next = order.filter(id => id !== card)
  next.splice(before === null ? next.length : next.indexOf(before), 0, card)
  return next
}
