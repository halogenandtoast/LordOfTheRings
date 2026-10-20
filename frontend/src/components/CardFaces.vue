<script setup lang="ts">
import { useServerText } from '../i18n'
import { cardImage, type CardView } from '../types'

// Cards shrunk to their art, stacked in a narrow column, as a small seat shows its zone.
defineProps<{ cards: CardView[] }>()
const tr = useServerText()
</script>

<template>
  <span class="card-faces">
    <span
      v-for="card in cards"
      :key="card.id"
      class="card-face"
      :title="tr(card.name)"
      :style="{ backgroundImage: `url(${cardImage(card)})` }"
    />
  </span>
</template>

<style scoped>
.card-faces {
  display: flex;
  flex-direction: column;
  align-self: center;
  width: 96px;
  margin: var(--gap);
  border-radius: 6px;
  overflow: hidden;
  background: rgb(0 0 0 / 0.22);
}
/* the face in the art, high on the card and right of the stats */
/* loose enough to catch it where the art puts it, from Aragorn's to Théodred's */
.card-face { aspect-ratio: 3 / 2; background-size: 180% auto; background-position: 89% 4%; }
.card-face + .card-face { box-shadow: 0 -1px 0 rgb(0 0 0 / 0.5); }
</style>
