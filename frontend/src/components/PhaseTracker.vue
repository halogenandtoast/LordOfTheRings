<script setup lang="ts">
defineProps<{ phase: string | null }>()

const phases = ['resource', 'planning', 'quest', 'travel', 'encounter', 'combat', 'refresh']
</script>

<template>
  <ol class="phases" :aria-label="$t('game.phaseTracker')">
    <li
      v-for="name in phases"
      :key="name"
      :class="{ active: name === phase }"
      :aria-current="name === phase ? 'step' : undefined"
    >
      {{ $t(`game.phases.${name}`) }}
    </li>
  </ol>
</template>

<style scoped>
.phases {
  display: flex;
  flex-direction: column;
  margin: 0;
  padding: 0;
  list-style: none;
  background: var(--surface-2);
  border: 1px solid var(--line);
  border-radius: 6px;
  overflow: hidden;
  user-select: none;
}
.phases li {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  writing-mode: vertical-rl;
  padding: 0.5rem 0.3rem;
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.12em;
  color: var(--cream-faint);
}
.phases li + li { border-top: 1px solid var(--line); }
@media (width < 1100px), (height < 500px) {
  .phases { flex-direction: row; overflow-x: auto; }
  .phases li { writing-mode: horizontal-tb; padding: 0.3rem 0.4rem; font-size: 0.6rem; letter-spacing: 0.06em; }
  .phases li + li { border-top: 0; border-left: 1px solid var(--line); }
}
.phases li.active {
  background: var(--forest);
  color: var(--accent-bright);
  font-weight: 700;
}
</style>
