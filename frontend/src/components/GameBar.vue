<script setup lang="ts">
import GameMenu from './GameMenu.vue'

defineProps<{ canUndo: boolean; showSidebar: boolean }>()
const emit = defineEmits<{ reset: []; undo: []; toggleSidebar: [] }>()
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
  background: var(--background-mid);
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
  color: rgb(255 255 255 / 0.55);
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
.game-bar :deep(.menu-items button:hover:not(:disabled)) { background: rgb(0 0 0 / 0.4); }
.game-bar :deep(.menu-items button:disabled) { opacity: 0.4; cursor: not-allowed; }

.right {
  display: flex;
  margin-left: auto;
}
.right .drawer-toggle {
  justify-content: center;
  min-width: 40px;
}
.right .drawer-toggle svg { width: 20px; height: 20px; }
.right .drawer-toggle:hover { background: rgb(0 0 0 / 0.4); }
.right .drawer-toggle[aria-expanded='true'] { background: rgb(0 0 0 / 0.21); }
</style>
