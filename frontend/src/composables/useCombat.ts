import { computed, inject, provide, ref, type ComputedRef, type InjectionKey } from 'vue'
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

const stat = (card: CardView | undefined, name: string) => card?.stats.find(([s]) => s === name)?.[1] ?? 0
const remaining = (card: CardView) => (card.hitPoints ?? 0) - card.damage

const key: InjectionKey<ComputedRef<Record<string, CombatPreview>>> = Symbol('combat')

export const provideCombat = (question: () => QuestionView | null, cards: () => CardView[]) =>
  provide(
    key,
    computed(() => {
      const q = question()
      if (!q) return {}
      const byId = new Map(cards().map((c) => [c.id, c]))
      const hovered = q.choices.some((c) => c.targetId === hoveredId.value) ? byId.get(hoveredId.value!) : undefined
      const prompt = q.prompt.split(' ')[0]

      if ((prompt === '$label.declareDefender' || prompt === '$label.declareDefenders') && q.sourceId && hovered) {
        // with several defenders, those already declared defend too
        const defense = [...q.targetIds.map((id) => byId.get(id)), hovered].reduce((sum, d) => sum + stat(d, 'defense'), 0)
        const damage = Math.max(0, stat(byId.get(q.sourceId), 'attack') - defense)
        return { [hovered.id]: { defend: { defense, damage, defeated: damage >= remaining(hovered) } } }
      }

      // an undefended attack's full damage, on the hero it would go to
      if (prompt === '$label.undefendedDamage' && q.sourceId && hovered) {
        const damage = stat(byId.get(q.sourceId), 'attack')
        return { [hovered.id]: { defend: { defense: null, damage, defeated: damage >= remaining(hovered) } } }
      }

      const enemy = prompt === '$label.declareAttackers' ? byId.get(q.targetIds[0]) : undefined
      if (enemy) {
        const strength = enemy.attackStrength ?? 0
        const defense = stat(enemy, 'defense')
        // ponytail: the hovered attacker's printed-side attack, without its bonuses against traits
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

export const useCombat = () => inject(key, computed((): Record<string, CombatPreview> => ({})))
