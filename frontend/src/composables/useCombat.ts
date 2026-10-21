import { computed, inject, provide, ref, type ComputedRef, type InjectionKey } from 'vue'
import type { CardView, QuestionView } from '../types'

/**
 * What a combat decision would come to, from the engine's stats: hovering a defender
 * shows its defense against the attack, and whether it would fall, before shadow effects, and an enemy being attacked
 * shows how much of its remaining hit points the declared attackers would deal.
 */
export interface CombatPreview {
  defend?: { defense: number; attack: number; defeated: boolean }
  /** `adding` is the hovered attacker's share, on top of what is `dealt` already. */
  attack?: { dealt: number; adding: number; remaining: number }
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
        const attack = stat(byId.get(q.sourceId), 'attack')
        return { [hovered.id]: { defend: { defense, attack, defeated: attack - defense >= remaining(hovered) } } }
      }

      const enemy = prompt === '$label.declareAttackers' ? byId.get(q.targetIds[0]) : undefined
      if (enemy) {
        const strength = enemy.attackStrength ?? 0
        const defense = stat(enemy, 'defense')
        const dealt = Math.max(0, strength - defense)
        // ponytail: the hovered attacker's printed-side attack, without its bonuses against traits
        const adding = hovered ? Math.max(0, strength + stat(hovered, 'attack') - defense) - dealt : 0
        return { [enemy.id]: { attack: { dealt, adding, remaining: remaining(enemy) } } }
      }
      return {}
    }),
  )

export const useCombat = () => inject(key, computed((): Record<string, CombatPreview> => ({})))
