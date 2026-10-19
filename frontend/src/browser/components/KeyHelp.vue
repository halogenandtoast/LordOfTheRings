<script setup lang="ts">
/**
 * The keys this view answers to.
 *
 * Rendered from the same array that binds them, so the list cannot go stale:
 * see `useShortcuts` in `@browser/lib/keys`.
 *
 * A native `<dialog>`, for the reasons `CardDialog` gives — focus trapping,
 * Escape, the top layer and a real backdrop, without a library. It marks itself
 * `data-shortcuts` so `shouldIgnore` lets the key that opened it through; every
 * other open dialog swallows shortcuts instead.
 */
import { ref, watch } from 'vue'
import type { Shortcut } from '@browser/lib/keys'

const { open, shortcuts } = defineProps<{
  open: boolean
  shortcuts: Shortcut[]
}>()

const emit = defineEmits<{ close: [] }>()

const dialog = ref<HTMLDialogElement | null>(null)

// Driven by the prop rather than by calling showModal from a click, so whatever
// opens it — a key, a button — goes through one path.
watch(
  () => open,
  (isOpen) => {
    const el = dialog.value
    if (!el) return
    if (isOpen && !el.open) el.showModal()
    else if (!isOpen && el.open) el.close()
  },
  { flush: 'post' },
)

/** A click on the dialog element itself landed on the backdrop, not the panel. */
function onClick(event: MouseEvent): void {
  if (event.target === dialog.value) emit('close')
}
</script>

<template>
  <dialog
    ref="dialog"
    class="dialog"
    data-shortcuts
    aria-label="Keyboard shortcuts"
    @cancel.prevent="emit('close')"
    @close="emit('close')"
    @click="onClick"
  >
    <div class="dialog__frame">
      <header class="dialog__bar">
        <h2 class="dialog__title">Keyboard</h2>
        <button type="button" class="button button--ghost" title="Close" @click="emit('close')">
          <span aria-hidden="true">✕</span>
          <span class="visually-hidden">Close</span>
        </button>
      </header>

      <dl class="keys">
        <template v-for="shortcut in shortcuts" :key="shortcut.key">
          <dt>
            <kbd>{{ shortcut.key }}</kbd>
          </dt>
          <dd>{{ shortcut.description }}</dd>
        </template>
        <dt><kbd>Esc</kbd></dt>
        <dd>Close this list</dd>
      </dl>
    </div>
  </dialog>
</template>

<style scoped>
.dialog {
  width: min(24rem, 100vw - 2rem);
  max-width: none;
  padding: 0;
  border: 1px solid var(--c-border-strong);
  border-radius: var(--radius-lg);
  background: var(--c-bg);
  color: var(--c-text);
  box-shadow: var(--shadow-lg);
  overflow: hidden;
}

.dialog::backdrop {
  background: hsl(0 0% 0% / 0.6);
  backdrop-filter: blur(2px);
}

.dialog__bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--space-3);
  padding: var(--space-2) var(--space-3);
  border-bottom: 1px solid var(--c-border);
  background: var(--c-surface);
}

.dialog__title {
  margin: 0;
  font-size: var(--text-sm);
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--c-text-muted);
}

.keys {
  display: grid;
  grid-template-columns: auto 1fr;
  align-items: baseline;
  gap: var(--space-2) var(--space-3);
  margin: 0;
  padding: var(--space-3);
}

.keys dt {
  justify-self: end;
}

.keys dd {
  margin: 0;
  color: var(--c-text-muted);
  font-size: var(--text-sm);
}

kbd {
  display: inline-grid;
  place-items: center;
  min-width: 1.4em;
  padding: 0.05em 0.3em;
  border: 1px solid var(--c-border-strong);
  border-radius: var(--radius-sm);
  background: var(--c-surface-2);
  font-family: inherit;
  font-size: 0.95em;
  line-height: 1.4;
}
</style>
