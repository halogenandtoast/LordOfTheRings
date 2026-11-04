import { nextTick } from 'vue'

interface ViewTransition {
  finished: Promise<void>
  updateCallbackDone: Promise<void>
  skipTransition: () => void
}
type StartViewTransition = (callback: () => void | Promise<void>) => ViewTransition

const supported = (): StartViewTransition | null => {
  const start = (document as Document & { startViewTransition?: StartViewTransition }).startViewTransition
  return typeof start === 'function' ? start.bind(document) : null
}

/**
 * Applies a state change inside a view transition so cards morph between zones, falling
 * back to a plain update where the API is unavailable.
 */
/**
 * Whether state changes will animate through a view transition. Not under a modal dialog:
 * a transition paints above the top layer the dialog sits in, so cards it moves would
 * show through the dialog's dim backdrop.
 */
export const transitionsOn = () =>
  supported() !== null &&
  !window.matchMedia('(prefers-reduced-motion: reduce)').matches &&
  !document.querySelector('dialog:modal')

let running: ViewTransition | null = null

/**
 * Open a modal dialog, cutting short any transition still running: the dialog was not
 * captured when it began, so it would sit under the cards moving until it ended.
 */
export const showModal = (dialog: HTMLDialogElement) => {
  running?.skipTransition()
  dialog.showModal()
}

export const withViewTransition = async (apply: () => void | Promise<void>): Promise<void> => {
  const start = supported()
  if (!start || !transitionsOn()) {
    await apply()
    await nextTick()
    return
  }
  const transition = start(async () => {
    await apply()
    await nextTick()
  })
  running = transition
  await transition.finished.catch(() => undefined)
  if (running === transition) running = null
}
