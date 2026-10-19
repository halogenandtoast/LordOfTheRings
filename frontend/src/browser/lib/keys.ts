/**
 * Keyboard shortcuts, and the list that documents them.
 *
 * One array drives both the handler and the help overlay, so a key that works
 * and a key that is listed cannot drift apart: `useShortcuts` binds the array,
 * `KeyHelp` renders the same one.
 *
 * Keys are global to whatever view registered them, so the guards matter more
 * than the binding does — the rules for when a keystroke is *not* a shortcut
 * live in `shouldIgnore`, and they are the reason this is a function rather
 * than a `window.addEventListener` in a component.
 */
import { useEventListener } from '@vueuse/core'

export interface Shortcut {
  /** Matched against `KeyboardEvent.key`, so `'?'` rather than Shift and `/`. */
  key: string
  /** What the help overlay says this does. A sentence, not a label. */
  description: string
  run: () => void
}

/**
 * Whether a keystroke belongs to something other than a shortcut.
 *
 * Typing comes first, and so does the browser: a modified key is a browser or
 * system command, and a key pressed inside a field is text. An open dialog gets
 * the keyboard to itself — a card preview is a dialog, and `u` inside one should
 * not quietly take back a move behind it. The exception is the help overlay,
 * which marks itself `data-shortcuts` so that the key which opened it still
 * closes it.
 */
export function shouldIgnore(event: KeyboardEvent): boolean {
  if (event.isComposing) return true
  // Shift is not a modifier here: `?` is Shift and `/`.
  if (event.ctrlKey || event.metaKey || event.altKey) return true

  const target = event.target as HTMLElement | null
  if (target?.isContentEditable) return true
  // An ancestor walk rather than a tag check, the way `CardDialog` does it: the
  // event can land on something inside the field.
  if (target?.closest('input, textarea, select')) return true

  return document.querySelector('dialog[open]:not([data-shortcuts])') !== null
}

/**
 * Bind a list of shortcuts for as long as the calling component is mounted.
 *
 * `useEventListener` owns the teardown, so there is no `onUnmounted` to forget.
 */
export function useShortcuts(shortcuts: Shortcut[]): void {
  useEventListener(window, 'keydown', (event: KeyboardEvent) => {
    if (shouldIgnore(event)) return
    const shortcut = shortcuts.find((s) => s.key === event.key)
    if (!shortcut) return
    event.preventDefault()
    shortcut.run()
  })
}
