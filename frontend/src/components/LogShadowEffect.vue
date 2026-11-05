<script setup lang="ts">
import { computed, watchEffect } from 'vue'
import CardText from '../browser/components/CardText.vue'
import { useCardStore } from '../browser/stores/cards'
import type { LogPart } from '../gameLog'

const props = defineProps<{ body: LogPart[] }>()
const cards = useCardStore()
// Only reveal notices expose this text; dealt, hidden and effectless cards do not.
const cardCode = computed(() => {
  const notice = props.body.find(part => part.tag === 'LogI18n' && part.contents[0] === 'log.shadowRevealed')
  if (notice?.tag !== 'LogI18n') return null
  const card = notice.contents[1].card
  return card?.tag === 'LogRefPart' && !card.contents.faceDown ? card.contents.cardCode : null
})
watchEffect(() => {
  if (cardCode.value && cards.state === 'idle') void cards.load()
})
// Engine codes use the image identifier; browser codes identify the printed set.
const shadows = computed(() => {
  const result = new Map<string, string>()
  for (const card of cards.cards) {
    if (!card.shadow) continue
    result.set(card.code, card.shadow)
    for (const image of card.images) {
      if (image.face === 'a') result.set(image.file.replace(/\.avif$/, ''), card.shadow)
    }
  }
  return result
})
const text = computed(() => cardCode.value ? shadows.value.get(cardCode.value) : undefined)
</script>

<template>
  <div v-if="text" class="log-shadow-effect">
    <hr class="log-shadow-divider" />
    <CardText :text="text" />
  </div>
</template>

<style scoped>
.log-shadow-effect {
  margin: 0 22px 7px 15px;
  font-size: 0.95em;
  line-height: 1.5;
  color: var(--text-muted);
}
.log-shadow-divider {
  width: min(100%, 320px);
  height: 30px;
  margin: 5px auto 7px;
  border: 0;
  background: currentColor;
  mask: url('/images/shadow-divider.svg') center / contain no-repeat;
  opacity: 0.65;
}
</style>
