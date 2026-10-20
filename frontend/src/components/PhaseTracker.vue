<script setup lang="ts">
defineProps<{ phase: string | null }>()

const phases = ['resource', 'planning', 'quest', 'travel', 'encounter', 'combat', 'refresh']
// on a phone each phase is its icon, the current one keeping its name beside it
const icons: Record<string, string> = {
  resource: 'leadership',
  planning: 'planning',
  quest: 'willpower',
  travel: 'travel',
  encounter: 'saurons-eye',
  combat: 'attack',
  refresh: 'refresh',
}
</script>

<template>
  <ol class="phases" :aria-label="$t('game.phaseTracker')">
    <li
      v-for="name in phases"
      :key="name"
      :class="{ active: name === phase }"
      :aria-current="name === phase ? 'step' : undefined"
      :title="$t(`game.phases.${name}`)"
    >
      <span class="icon" :style="{ '--icon': `url('/images/icons/game/${icons[name]}.svg')` }" aria-hidden="true" />
      <span class="name">{{ $t(`game.phases.${name}`) }}</span>
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
.phases .icon { display: none; }
@media (width < 700px) {
  .phases { overflow-x: visible; }
  .phases li { flex: 1 1 0; gap: 5px; padding: 0.4rem 0.3rem; }
  .phases .icon { display: inline-block; font-size: 1.1rem; }
  .phases li:not(.active) .name { display: none; }
  .phases li.active { flex-grow: 2.4; }
}
.phases li.active {
  background: var(--forest);
  color: var(--accent-bright);
  font-weight: 700;
}
</style>
