import { computed, inject, provide, ref, watch, type ComputedRef, type InjectionKey } from 'vue'
import type { CardView, QuestionView } from '../types'

/**
 * What a combat decision would come to, from the engine's stats: hovering a defender
 * shows its defense and the damage that gets past it, and whether it would fall, before shadow effects, and an enemy being attacked
 * shows how far the declared attack gets through its defense and its remaining hit points.
 */
export interface CombatPreview {
  /** `damage` is what gets past its defense, none when the attack is undefended. */
  defend?: { defense: number | null; damage: number; defeated: boolean }
  /** The attack wearing down its defense, then its remaining hit points: each bar has
   * what is `done` already and the hovered attacker's share `adding` to it, out of `of`. */
  attack?: { bars: { stat: 'defense' | 'hit-points'; done: number; adding: number; of: number }[]; defeated: boolean }
}

/** The card under the mouse, so a choice can be previewed before it is made. */
export const hoveredId = ref<string | null>(null)
/** The enemy attackers were last declared against, kept through the action window before
 * the attack resolves. */
const attackedId = ref<string | null>(null)

const stat = (card: CardView | undefined, name: string) => card?.stats.find(([s]) => s === name)?.[1] ?? 0
const remaining = (card: CardView) => (card.hitPoints ?? 0) - card.damage

const key: InjectionKey<ComputedRef<Record<string, CombatPreview>>> = Symbol('combat')

const isAttacking = (q: QuestionView) => {
  const prompt = q.prompt.split(' ')[0]
  return prompt === '$label.declareAttackers' || prompt === '$label.joinAttackRanged'
}

export const provideCombat = (question: () => QuestionView | null, cards: () => CardView[]) => {
  watch(question, (q) => {
    if (q && isAttacking(q)) attackedId.value = q.targetIds[0] ?? null
  }, { immediate: true })
  provide(
    key,
    computed(() => {
      const q = question()
      const byId = new Map(cards().map((c) => [c.id, c]))
      const hovered = q?.choices.some((c) => c.targetId === hoveredId.value) ? byId.get(hoveredId.value!) : undefined
      const prompt = q?.prompt.split(' ')[0]

      if (q && (prompt === '$label.declareDefender' || prompt === '$label.declareDefenders') && q.sourceId && hovered) {
        // with several defenders, those already declared defend too
        const defense = [...q.targetIds.map((id) => byId.get(id)), hovered].reduce((sum, d) => sum + stat(d, 'defense'), 0)
        const damage = Math.max(0, stat(byId.get(q.sourceId), 'attack') - defense)
        return { [hovered.id]: { defend: { defense, damage, defeated: damage >= remaining(hovered) } } }
      }

      // an undefended attack's full damage, on the hero it would go to, with its bonuses
      // for going undefended, as Goblin Swordsman's
      if (q && prompt === '$label.undefendedDamage' && q.sourceId && hovered) {
        const damage = q.choices.find((c) => c.targetId === hovered.id)?.damage ?? stat(byId.get(q.sourceId), 'attack')
        return { [hovered.id]: { defend: { defense: null, damage, defeated: damage >= remaining(hovered) } } }
      }

      // anything else dealing damage to the card chosen, such as a when revealed effect
      const dealing = q?.choices.find((c) => c.targetId === hovered?.id)?.damage
      if (hovered && dealing) {
        return { [hovered.id]: { defend: { defense: null, damage: dealing, defeated: dealing >= remaining(hovered) } } }
      }

      // an enemy being attacked, while attackers are declared and through the action window
      // after, until the attack resolves and its attackers stop
      const attacked = attackedId.value ? byId.get(attackedId.value) : undefined
      const enemy = q && isAttacking(q) ? byId.get(q.targetIds[0]) : attacked?.attackStrength !== undefined ? attacked : undefined
      if (enemy) {
        const strength = enemy.attackStrength ?? 0
        const defense = stat(enemy, 'defense')
        const withHovered = strength + stat(hovered, 'attack')
        const bar = (stat: 'defense' | 'hit-points', of: number, through: (attack: number) => number) => ({
          stat, of, done: through(strength), adding: through(withHovered) - through(strength),
        })
        const left = remaining(enemy)
        const bars = [
          ...(defense > 0 ? [bar('defense', defense, (a) => Math.min(a, defense))] : []),
          bar('hit-points', left, (a) => Math.min(left, Math.max(0, a - defense))),
        ]
        return { [enemy.id]: { attack: { bars, defeated: withHovered - defense >= left } } }
      }
      return {}
    }),
  )
}

export const useCombat = () => inject(key, computed((): Record<string, CombatPreview> => ({})))
