import { computed, inject, provide, type ComputedRef, type InjectionKey } from 'vue'
import type { QuestionView } from '../types'

/**
 * The board's visual language for a decision: magenta for what you can interact with
 * (drawn from the choices), cyan for the card causing it and amber for what it targets.
 */
export interface Highlights {
  sourceId: string | null
  targetIds: string[]
}

const key: InjectionKey<ComputedRef<Highlights>> = Symbol('highlights')

export const provideHighlights = (question: () => QuestionView | null) =>
  provide(
    key,
    computed(() => ({ sourceId: question()?.sourceId ?? null, targetIds: question()?.targetIds ?? [] })),
  )

export const useHighlights = () => inject(key, computed(() => ({ sourceId: null, targetIds: [] })))
