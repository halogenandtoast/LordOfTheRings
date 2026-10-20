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
    <span v-if="label" class="band-label">{{ label }}</span>
    <slot />
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
.band.empty { flex: 0 0 auto; min-height: 0; padding: 0; border-color: transparent; }
</style>
