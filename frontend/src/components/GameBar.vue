<script setup lang="ts">
import GameMenu from './GameMenu.vue'

const undoShortcut = /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘Z' : 'Ctrl+Z'

defineProps<{ canUndo: boolean; showSidebar: boolean }>()
const emit = defineEmits<{ reset: []; undo: [to?: 'phase' | 'round']; export: []; toggleSidebar: [] }>()
</script>

<template>
  <div class="game-bar">
    <GameMenu>
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" d="M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5" />
      </svg>
      Game
      <template #items>
        <div class="menu-section">Scenario</div>
        <button type="button" @click="emit('reset')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M3 3v1.5M3 21v-6m0 0 2.77-.693a9 9 0 0 1 6.208.682l.108.054a9 9 0 0 0 6.086.71l3.114-.732a48.524 48.524 0 0 1-.005-10.499l-3.11.732a9 9 0 0 1-6.085-.711l-.108-.054a9 9 0 0 0-6.208-.682L3 4.5M3 15V4.5" />
          </svg>
          Restart scenario
        </button>
        <div class="menu-section">File</div>
        <button type="button" @click="emit('export')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M3 16.5v2.25A2.25 2.25 0 0 0 5.25 21h13.5A2.25 2.25 0 0 0 21 18.75V16.5M16.5 12 12 16.5m0 0L7.5 12m4.5 4.5V3" />
          </svg>
          Export game
        </button>
      </template>
    </GameMenu>
    <GameMenu>
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" d="M21 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.061a1.125 1.125 0 0 1 0-1.954l7.108-4.061A1.125 1.125 0 0 1 21 8.689v8.122ZM11.25 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.061a1.125 1.125 0 0 1 0-1.954L9.567 7.71a1.125 1.125 0 0 1 1.683.977v8.122Z" />
      </svg>
      Undo
      <template #items>
        <button type="button" :disabled="!canUndo" @click="emit('undo')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M9 15 3 9m0 0 6-6M3 9h12a6 6 0 0 1 0 12h-3" />
          </svg>
          Undo last answer
          <span class="shortcut">{{ undoShortcut }}</span>
        </button>
        <div class="menu-section">Undo to</div>
        <button type="button" :disabled="!canUndo" @click="emit('undo', 'phase')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M6.429 9.75 2.25 12l4.179 2.25m0-4.5 5.571 3 5.571-3m-11.142 0L2.25 7.5 12 2.25l9.75 5.25-4.179 2.25m0 0L21.75 12l-4.179 2.25m0 0 4.179 2.25L12 21.75 2.25 16.5l4.179-2.25m11.142 0-5.571 3-5.571-3" />
          </svg>
          Start of phase
        </button>
        <button type="button" :disabled="!canUndo" @click="emit('undo', 'round')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0 3.181 3.183a8.25 8.25 0 0 0 13.803-3.7M4.031 9.865a8.25 8.25 0 0 1 13.803-3.7l3.181 3.182m0-4.991v4.99" />
          </svg>
          Start of round
        </button>
      </template>
    </GameMenu>
    <div class="right">
      <button
        type="button"
        class="drawer-toggle"
        aria-label="Toggle sidebar"
        title="Toggle sidebar"
        :aria-expanded="showSidebar"
        @click="emit('toggleSidebar')"
      >
        <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
          <rect x="3" y="4" width="18" height="16" rx="2" />
          <path d="M15 4v16" />
          <path :d="showSidebar ? 'm8 9 3 3-3 3' : 'm11 9-3 3 3 3'" />
        </svg>
      </button>
    </div>
  </div>
</template>

<style scoped>
.game-bar {
  display: flex;
  justify-content: flex-start;
  margin: 0;
  padding: 0;
  background: var(--bar-background);
  border-bottom: 1px solid var(--line);
  box-shadow: 0 2px 8px var(--shadow);
  font-size: 0.875rem;
  flex-shrink: 0;
  position: relative;
  z-index: 50;
}

.game-bar :deep(svg) { width: 15px; }

.menu-section {
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--menu-section-text);
  padding: 6px 10px 4px;
  user-select: none;
  pointer-events: none;
}

.right button {
  background: none;
  border: 0;
  border-radius: 0;
  color: inherit;
  font: inherit;
  padding: 5px 10px;
  display: flex;
  gap: 5px;
  height: 100%;
  align-items: center;
  cursor: pointer;
}
.game-bar :deep(.menu-items button) {
  background: none;
  border: 0;
  border-radius: 0;
  color: inherit;
  font: inherit;
  padding: 8px 10px;
  display: flex;
  gap: 8px;
  align-items: center;
  cursor: pointer;
  text-align: left;
}
.game-bar :deep(.menu-items button:hover:not(:disabled)) { background: var(--hover-tint); }
.game-bar :deep(.menu-items button:disabled) { opacity: 0.4; cursor: not-allowed; }

.shortcut {
  margin-left: auto;
  padding-left: 16px;
  font-size: 0.8em;
  color: var(--text-muted);
}

.right {
  display: flex;
  align-items: center;
  margin-left: auto;
}
.right .drawer-toggle {
  justify-content: center;
  min-width: 40px;
}
.right .drawer-toggle svg { width: 20px; height: 20px; }
.right .drawer-toggle:hover { background: var(--hover-tint); }
</style>
