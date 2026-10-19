<script setup lang="ts">
import { computed, ref, useTemplateRef } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { cardImage, type CardView, type ChoiceView } from '../types'

const { card, choices = [], attachments = [], rotated = false, flipping = false } = defineProps<{
  card: CardView
  choices?: ChoiceView[]
  attachments?: CardView[]
  rotated?: boolean
  flipping?: boolean
}>()

const emit = defineEmits<{ choose: [index: number] }>()

const overlay = useCardOverlay()
const expanded = ref(false)
const cardRef = useTemplateRef<HTMLElement>('cardRef')

const source = computed(() => cardImage(card))
const choice = computed(() => choices.find((option) => option.targetId === card.id) ?? null)
const active = computed(() => choice.value !== null)
const choiceFor = (attached: CardView) =>
  choices.find((option) => option.targetId === attached.id) ?? null

const choose = () => {
  if (choice.value) {
    emit('choose', choice.value.index)
  }
}

const toggleUnless = (attached: CardView) => {
  if (!choiceFor(attached)) {
    expanded.value = !expanded.value
  }
}

</script>

<template>
  <div class="card-slot" :class="{ expanded, hosting: attachments.length > 0 }">
    <div v-if="attachments.length" class="attachments">
      <div
        v-for="(attached, index) in attachments"
        :key="attached.id"
        class="attached"
        :style="{ '--depth': attachments.length - index }"
        @click="toggleUnless(attached)"
      >
        <GameCard :card="attached" :choices="choices" @choose="emit('choose', $event)" />
      </div>
    </div>

    <div
      ref="cardRef"
      class="card"
      :class="{ rotated, exhausted: card.exhausted, active, committed: card.committed, flipping }"
      :style="{ viewTransitionName: `card-${card.id}` }"
      :title="card.name"
      @mouseenter="cardRef && overlay.show(card, cardRef, rotated)"
      @mouseleave="overlay.hide()"
      @click="choose"
    >
      <img :src="source" :alt="card.name" />
      <span v-if="card.resources" class="token resources">{{ card.resources }}</span>
      <span v-if="card.progress" class="token progress">{{ card.progress }}</span>
      <span v-if="card.damage > 0" class="token damage">{{ card.damage }}</span>
    </div>
  </div>
</template>

<style scoped>
.card-slot { position: relative; line-height: 0; }
.card-slot.hosting { padding-top: calc(var(--attachment-peek) * v-bind('attachments.length')); }
.card-slot.hosting.expanded {
  padding-top: calc(var(--card-width) / var(--card-aspect) * v-bind('attachments.length') * 0.55);
}

.attachments {
  position: absolute;
  inset: 0 0 auto 0;
  display: grid;
  z-index: 1;
}
.attached {
  grid-area: 1 / 1;
  cursor: pointer;
  transform: translateY(calc(var(--depth) * var(--attachment-peek) * -1 + var(--attachment-peek)));
  transition: transform 0.2s ease;
  filter: drop-shadow(0 -1px 4px var(--shadow));
}
.card-slot.expanded .attached {
  transform: translateY(calc(var(--depth) * (var(--card-width) / var(--card-aspect)) * -0.55
    + (var(--card-width) / var(--card-aspect)) * 0.55));
}

.card {
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  display: grid;
  position: relative;
  isolation: isolate;
  z-index: 2;
}
.card.rotated {
  width: calc(var(--card-width) / var(--card-aspect));
  aspect-ratio: calc(1 / 0.715);
}
.card img { width: 100%; border-radius: 5px; grid-area: 1 / 1; }
.card.exhausted { transform: rotate(90deg) scale(0.88); }
.card.committed img { box-shadow: 0 0 0 2px var(--accent); }
.card.active { cursor: pointer; }
.card.active img { border: 2px solid var(--select); }
.card.flipping { animation: flip 0.32s ease-in-out; }
.card.flipping.exhausted { animation: flip-exhausted 0.32s ease-in-out; }

@keyframes flip {
  0% { transform: rotateY(0deg); }
  50% { transform: rotateY(90deg) scale(1.04); }
  100% { transform: rotateY(0deg); }
}

@keyframes flip-exhausted {
  0% { transform: rotate(90deg) scale(0.88) rotateY(0deg); }
  50% { transform: rotate(90deg) scale(0.92) rotateY(90deg); }
  100% { transform: rotate(90deg) scale(0.88) rotateY(0deg); }
}

.token {
  z-index: 3;
  display: grid;
  place-items: center;
  background-size: cover;
  width: calc(var(--card-width) / 4);
  height: calc(var(--card-width) / 4);
  color: var(--token-text);
  font-weight: 700;
  font-size: 0.7rem;
  line-height: 1;
  grid-area: 1 / 1;
}
.resources { background-image: url('/images/tokens/resource.png'); place-self: end start; }
.progress { background-image: url('/images/tokens/progress.png'); place-self: center; }
.damage {
  place-self: start end;
  background: var(--danger);
  color: var(--damage-text);
  border-radius: 50%;
  border: 1px solid var(--shadow);
}

</style>
