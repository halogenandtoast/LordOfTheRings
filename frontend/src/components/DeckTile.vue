<script setup lang="ts">
import type { DeckFace } from '../deckChoice'

// A deck shown by its heroes' art, named underneath.
defineProps<{ face: DeckFace; selected?: boolean }>()
</script>

<template>
  <span class="deck-tile" :class="{ selected }" :data-sphere="face.sphere ?? undefined">
    <span class="heroes">
      <span v-for="code in face.heroCodes" :key="code" class="hero" :style="{ backgroundImage: `url(/images/cards/${code}.jpg)` }" />
    </span>
    <span class="deck-name">{{ face.sphere ? $t(`newGame.starter.${face.sphere}`) : face.name }}</span>
    <span class="deck-detail">{{ face.sphere ? $t('newGame.starterDeck') : face.detail }}</span>
  </span>
</template>

<style scoped>
.deck-tile {
  --sphere-color: var(--accent);
  display: flex;
  flex-direction: column;
  min-width: 0;
  border: 2px solid transparent;
  border-radius: 6px;
  overflow: hidden;
  background: var(--input-background);
  text-align: left;
  transition: border-color 0.15s, transform 0.15s;
}
.deck-tile[data-sphere='leadership'] { --sphere-color: var(--leadership); }
.deck-tile[data-sphere='tactics'] { --sphere-color: hsl(4 66% 56%); }
.deck-tile[data-sphere='spirit'] { --sphere-color: hsl(200 50% 70%); }
.deck-tile[data-sphere='lore'] { --sphere-color: hsl(96 34% 50%); }
.deck-tile.selected { border-color: var(--accent); }
.heroes {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  aspect-ratio: 2 / 1;
  border-bottom: 3px solid var(--sphere-color);
  background: var(--surface-2);
}
/* the art sits in the top half of a card, right of the stats */
.hero { background-size: 210% auto; background-position: 62% 14%; }
.hero + .hero { box-shadow: -1px 0 0 rgb(0 0 0 / 0.5); }
.deck-name { padding: 6px 8px 0; font-weight: 700; color: var(--text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.deck-detail { padding: 0 8px 7px; font-size: 0.75rem; color: var(--text-muted); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
</style>
