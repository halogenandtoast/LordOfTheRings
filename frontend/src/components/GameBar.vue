<script setup lang="ts">
import GameMenu from './GameMenu.vue'

const undoShortcut = /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘Z' : 'Ctrl+Z'

defineProps<{ canUndo: boolean; showSidebar: boolean; debugging: boolean }>()
const emit = defineEmits<{ reset: []; undo: [to?: 'phase' | 'round']; export: []; fileBug: []; toggleSidebar: []; toggleDebug: [] }>()
</script>

<template>
  <div class="game-bar">
    <GameMenu>
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" d="M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5" />
      </svg>
      {{ $t('common.game') }}
      <template #items>
        <div class="menu-section">{{ $t('common.scenario') }}</div>
        <button type="button" @click="emit('reset')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M3 3v1.5M3 21v-6m0 0 2.77-.693a9 9 0 0 1 6.208.682l.108.054a9 9 0 0 0 6.086.71l3.114-.732a48.524 48.524 0 0 1-.005-10.499l-3.11.732a9 9 0 0 1-6.085-.711l-.108-.054a9 9 0 0 0-6.208-.682L3 4.5M3 15V4.5" />
          </svg>
          {{ $t('game.bar.restartScenario') }}
        </button>
        <div class="menu-section">{{ $t('game.bar.file') }}</div>
        <button type="button" @click="emit('export')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M3 16.5v2.25A2.25 2.25 0 0 0 5.25 21h13.5A2.25 2.25 0 0 0 21 18.75V16.5M16.5 12 12 16.5m0 0L7.5 12m4.5 4.5V3" />
          </svg>
          {{ $t('game.bar.exportGame') }}
        </button>
        <button type="button" @click="emit('fileBug')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M12 9v3.75m-9.303 3.376c-.866 1.5.217 3.374 1.948 3.374h14.71c1.73 0 2.813-1.874 1.948-3.374L13.949 3.378c-.866-1.5-3.032-1.5-3.898 0L2.697 16.126ZM12 15.75h.007v.008H12v-.008Z" />
          </svg>
          {{ $t('game.bar.fileBug') }}
        </button>
        <div class="menu-section">{{ $t('game.bar.testing') }}</div>
        <button type="button" :aria-pressed="debugging" @click="emit('toggleDebug')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M12 12.75c1.148 0 2.278.08 3.383.237 1.037.146 1.866.966 1.866 2.013 0 3.728-2.35 6.75-5.25 6.75S6.75 18.728 6.75 15c0-1.046.83-1.867 1.866-2.013A24.204 24.204 0 0 1 12 12.75Zm0 0c2.883 0 5.647.508 8.207 1.44a23.91 23.91 0 0 1-1.152 6.06M12 12.75c-2.883 0-5.647.508-8.208 1.44.125 2.104.52 4.136 1.153 6.06M12 12.75a2.25 2.25 0 0 0 2.248-2.354M12 12.75a2.25 2.25 0 0 1-2.248-2.354M12 8.25c.995 0 1.971-.08 2.922-.236.403-.066.74-.358.795-.762a3.778 3.778 0 0 0-.399-2.25M12 8.25c-.995 0-1.97-.08-2.922-.236-.402-.066-.74-.358-.795-.762a3.734 3.734 0 0 1 .4-2.253M12 8.25a2.25 2.25 0 0 0-2.248 2.146M12 8.25a2.25 2.25 0 0 1 2.248 2.146M8.683 5a6.032 6.032 0 0 1-1.155-1.002c.07-.63.27-1.222.574-1.747m.581 2.749A3.75 3.75 0 0 1 15.318 5m0 0c.427-.283.815-.62 1.155-.999a4.471 4.471 0 0 0-.575-1.752M4.921 6a24.048 24.048 0 0 0-.392 3.314c1.668.546 3.416.914 5.223 1.082M19.08 6c.205 1.08.337 2.187.392 3.314a23.882 23.882 0 0 1-5.223 1.082" />
          </svg>
          {{ debugging ? $t('game.bar.leaveDebugMode') : $t('game.bar.debugMode') }}
          <span class="shortcut">D</span>
        </button>
      </template>
    </GameMenu>
    <GameMenu>
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" d="M21 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.061a1.125 1.125 0 0 1 0-1.954l7.108-4.061A1.125 1.125 0 0 1 21 8.689v8.122ZM11.25 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.061a1.125 1.125 0 0 1 0-1.954L9.567 7.71a1.125 1.125 0 0 1 1.683.977v8.122Z" />
      </svg>
      {{ $t('game.bar.undo') }}
      <template #items>
        <button type="button" :disabled="!canUndo" @click="emit('undo')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M9 15 3 9m0 0 6-6M3 9h12a6 6 0 0 1 0 12h-3" />
          </svg>
          {{ $t('game.bar.undoLastAnswer') }}
          <span class="shortcut">{{ undoShortcut }}</span>
        </button>
        <div class="menu-section">{{ $t('game.bar.undoTo') }}</div>
        <button type="button" :disabled="!canUndo" @click="emit('undo', 'phase')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M6.429 9.75 2.25 12l4.179 2.25m0-4.5 5.571 3 5.571-3m-11.142 0L2.25 7.5 12 2.25l9.75 5.25-4.179 2.25m0 0L21.75 12l-4.179 2.25m0 0 4.179 2.25L12 21.75 2.25 16.5l4.179-2.25m11.142 0-5.571 3-5.571-3" />
          </svg>
          {{ $t('game.bar.startOfPhase') }}
        </button>
        <button type="button" :disabled="!canUndo" @click="emit('undo', 'round')">
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0 3.181 3.183a8.25 8.25 0 0 0 13.803-3.7M4.031 9.865a8.25 8.25 0 0 1 13.803-3.7l3.181 3.182m0-4.991v4.99" />
          </svg>
          {{ $t('game.bar.startOfRound') }}
        </button>
      </template>
    </GameMenu>
    <div class="right">
      <button
        type="button"
        class="drawer-toggle"
        :aria-label="$t('game.bar.toggleSidebar')"
        :title="$t('game.bar.toggleSidebar')"
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
