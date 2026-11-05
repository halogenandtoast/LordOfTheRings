import type { Question } from './server'

/** Resources already allocated in a payment that the engine will spend together.
 * Each offered choice prepends its next hero to the allocation, so the tail is
 * the payment chosen so far. Read one choice per question, never all alternatives.
 */
export const pendingResourcePayments = (questions: Record<string, Question>): Map<string, number> => {
  const pending = new Map<string, number>()
  for (const question of Object.values(questions)) {
    let q = question
    while ('question' in q) q = q.question
    const messages = q.choices[0]?.messages ?? []
    for (const message of messages) {
      const index = message.tag === 'SpendResourcesChosen' ? 4 : message.tag === 'SpendResourcesFromDistinctHeroes' ? 3 : null
      if (index === null || !Array.isArray(message.contents)) continue
      const chosen: unknown = message.contents[index]
      if (!Array.isArray(chosen)) continue
      for (const hero of chosen.slice(1)) {
        if (typeof hero === 'string') pending.set(hero, (pending.get(hero) ?? 0) + 1)
      }
      break
    }
  }
  return pending
}
