<script setup lang="ts">
import type { ScenarioOption } from '../campaigns'

defineProps<{ scenarios: ScenarioOption[] }>()
</script>

<template>
  <ol class="scenario-preview" role="list" :aria-label="$t('campaignMode.scenarios')">
    <li v-for="(scenario, index) in scenarios" :key="scenario.id" class="preview-box" :class="{ beta: scenario.beta, alpha: scenario.alpha }">
      <span class="scenario-number" aria-hidden="true">{{ index + 1 }}</span>
      <img :src="scenario.image" alt="" />
      <span class="scenario-name"><span class="icon" :style="{ '--icon': `url(${scenario.icon})` }" />{{ scenario.name }}</span>
    </li>
  </ol>
</template>

<style scoped>
.scenario-preview {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(0, 128px));
  gap: 12px;
  margin: 0;
  padding: 0;
  list-style: none;
}
.preview-box {
  position: relative;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  border: 2px solid transparent;
  border-radius: 5px;
  background: var(--input-background);
  color: var(--text);
}
.preview-box img { width: 100%; aspect-ratio: 1; box-sizing: border-box; padding: 8px; object-fit: contain; display: block; }
.scenario-name { display: flex; align-items: center; justify-content: center; gap: 6px; padding: 8px; font-size: 0.8rem; font-weight: 600; line-height: 1.35; text-align: center; }
.scenario-name .icon { font-size: 1.2em; }
.scenario-number { position: absolute; top: 8px; right: 8px; display: grid; place-items: center; min-width: 24px; height: 24px; background: var(--box-background); color: var(--text); border: 1px solid var(--box-border); border-radius: 3px; font-size: 0.75rem; font-weight: 700; font-variant-numeric: tabular-nums; }
.preview-box.beta::after, .preview-box.alpha::after { position: absolute; top: 9px; left: -22px; width: 86px; rotate: -45deg; color: var(--cream); font-size: 0.7rem; font-weight: 700; letter-spacing: 0.08em; line-height: 24px; text-align: center; text-transform: uppercase; box-shadow: 0 4px 10px var(--shadow); }
.preview-box.beta::after { content: 'beta'; background: darkgoldenrod; }
.preview-box.alpha::after { content: 'alpha'; background: darkred; }
</style>
