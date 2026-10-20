<script setup lang="ts">
import { computed } from 'vue'
import GameCard from './GameCard.vue'
import type { CardView, ChoiceView } from '../types'

const { cards, choices, flipping, align = 'top' } = defineProps<{
  cards: CardView[]
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  label?: string
  align?: 'top' | 'bottom'
}>()
const emit = defineEmits<{ choose: [index: number] }>()

const hosts = computed(() => cards.filter((card) => card.attachedTo === null))
const attachmentsFor = (card: CardView) => cards.filter((other) => other.attachedTo === card.id)
</script>

<template>
  <section class="band" :class="[`align-${align}`, { empty: !cards.length }]">
    <span v-if="label && cards.length" class="band-label">{{ label }}</span>
    <GameCard
      v-for="card in hosts"
      :key="card.id"
      :card="card"
      :choices="choices"
      :attachments="attachmentsFor(card)"
      :flipping="flipping.has(card.id)"
      @choose="emit('choose', $event)"
    />
  </section>
</template>

<style scoped>
.band { flex: 1 1 auto; }
.band.align-top { align-content: flex-start; align-items: flex-start; }
.band.align-bottom { align-content: flex-end; align-items: flex-end; }
/* centred cards leave the label at the band's edge */
.band.centered { position: relative; }
.band.centered .band-label { position: absolute; left: 4px; top: 50%; translate: 0 -50%; }
.band.empty { flex: 0 0 auto; min-height: 0; padding: 0; border-color: transparent; }
.band-label {
  align-self: center;
  font-size: 0.6rem;
  text-transform: uppercase;
  letter-spacing: 0.12em;
  opacity: 0.35;
  writing-mode: vertical-rl;
  rotate: 180deg;
}
</style>
