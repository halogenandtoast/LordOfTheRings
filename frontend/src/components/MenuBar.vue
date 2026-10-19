<script setup lang="ts">
import { ref } from 'vue'

const { disabled = false, canUndo = false } = defineProps<{ disabled?: boolean; canUndo?: boolean }>()
const emit = defineEmits<{ reset: []; undo: [] }>()

const open = ref(false)

const pick = (action: () => void) => {
  open.value = false
  action()
}
</script>

<template>
  <nav class="menubar" @mouseleave="open = false">
    <button type="button" class="menu-title" :aria-expanded="open" @click="open = !open">Game</button>
    <ul v-if="open" class="menu">
      <li>
        <button type="button" :disabled="!canUndo" @click="pick(() => emit('undo'))">
          Undo
          <span class="shortcut">last answer</span>
        </button>
      </li>
      <li><hr /></li>
      <li>
        <button type="button" :disabled="disabled" @click="pick(() => emit('reset'))">
          Restart scenario
        </button>
      </li>
    </ul>
  </nav>
</template>

<style scoped>
.menubar {
  position: relative;
  display: flex;
  gap: 0.25rem;
  align-items: center;
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 5px;
  padding: 2px;
  z-index: 50;
}
.menu-title {
  font: inherit;
  font-size: 0.75rem;
  padding: 0.2rem 0.6rem;
  border: 0;
  border-radius: 3px;
  background: transparent;
  color: inherit;
  cursor: pointer;
}
.menu-title:hover, .menu-title[aria-expanded='true'] { background: rgb(226 205 145 / 0.14); }
.menu {
  position: absolute;
  top: 100%;
  left: 0;
  margin: 2px 0 0;
  padding: 3px;
  list-style: none;
  min-width: 11rem;
  background: rgb(12 10 8 / 0.96);
  border: 1px solid var(--line);
  border-radius: 5px;
  box-shadow: 0 6px 20px rgb(0 0 0 / 0.6);
}
.menu button {
  width: 100%;
  text-align: left;
  font: inherit;
  font-size: 0.75rem;
  padding: 0.3rem 0.5rem;
  border: 0;
  border-radius: 3px;
  background: transparent;
  color: inherit;
  cursor: pointer;
}
.menu button:hover:not(:disabled) { background: rgb(226 205 145 / 0.16); }
.menu button:disabled { opacity: 0.4; cursor: not-allowed; }
.menu button { display: flex; justify-content: space-between; gap: 1.5rem; align-items: baseline; }
.shortcut { font-size: 0.65rem; opacity: 0.45; }
.menu hr { border: 0; border-top: 1px solid var(--line); margin: 3px 2px; }
</style>
