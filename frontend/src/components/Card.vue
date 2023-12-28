<script setup lang="ts">
import { ref, watch, computed } from 'vue'
import { Card, Question, Choice } from '@/types/Game'
const props = defineProps<{ card: Card, resources?: number, data: GameData }>()
const emit = defineEmits<{ choose: [string] }>()

const showBig = ref(false)
const bigRef = ref(null)
const cardRef = ref(null)

const question = computed(() => {
  return props.data.game.players[props.data.playerId].question
})

const choose = () => {
  if (choice.value) {
    emit('choose', choice.value)
  }
}

const choice = computed(() => {
  const currentQuestion = question.value
  if (!currentQuestion) return null

  switch (currentQuestion.tag) {
    case "ChooseOne":
      return currentQuestion.contents.find((choice: Choice) => choice.cardId === props.card.id)
    default:
      return null
  }
})

const active = computed(() => {
  const currentChoice = choice.value
  return currentChoice !== undefined && currentChoice !== null
})

watch(bigRef, () => {
  const card = cardRef.value
  const big = bigRef.value
  if (card && big) {
    const maxWidth = window.innerWidth
    const cardRect = card.getBoundingClientRect()
    const bigRect = big.getBoundingClientRect()
    let x = cardRect.x

    if (x + bigRect.width > maxWidth) {
      x = x - bigRect.width + cardRect.width
    }

    const y = cardRect.y - bigRect.height
    big.style.left = `${x}px`
    big.style.top = `${y}px`
  }
})

</script>

<template>
  <div>
    <div ref="cardRef" class="card" :id="card.id" @mouseover="showBig = true" @mouseleave="showBig = false" :class="{ active }" @click="choose">
      <img :src="`/images/cards/${card.def.code}.jpg`" />
      <span v-if="resources" class="resources">{{resources}}</span>
    </div>
    <img ref="bigRef" class="big" v-if="showBig" :src="`/images/cards/${card.def.code}.jpg`" :anchor="card.id" />
  </div>
</template>

<style scoped lang="scss">
.card {
  width: $card-width;
  height: fit-content;
  line-height: 0;
  display: grid;
  position: relative;
  isolation: isolate;
}

.card img {
  width: 100%;
  border-radius: 5px;
  grid-area: 1 / 1;
}

.resources {
  background-image: url('/images/tokens/resource.png');
  display: grid;
  place-items: center;
  background-size: cover;
  width: calc($card-width / 4);
  height: calc($card-width / 4);
  color: black;
  font-weight: bold;
  grid-area: 1 / 1;
  place-self: center;
}

img.big {
  position: absolute;
  width: calc($card-width * 4);
  height: fit-content;
  aspect-ratio: 429/600;
  border-radius: 20px;
}

.active {
  border: 2px solid red;
  box-sizing: border-box;
}
</style>
