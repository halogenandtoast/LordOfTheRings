import { nextTick } from 'vue'

type StartViewTransition = (callback: () => void | Promise<void>) => {
  finished: Promise<void>
  updateCallbackDone: Promise<void>
}

const supported = (): StartViewTransition | null => {
  const start = (document as Document & { startViewTransition?: StartViewTransition }).startViewTransition
  return typeof start === 'function' ? start.bind(document) : null
}

/**
 * Applies a state change inside a view transition so cards morph between zones, falling
 * back to a plain update where the API is unavailable.
 */
export const withViewTransition = async (apply: () => void): Promise<void> => {
  const start = supported()
  if (!start || window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
    apply()
    await nextTick()
    return
  }
  const transition = start(async () => {
    apply()
    await nextTick()
  })
  await transition.finished.catch(() => undefined)
}
