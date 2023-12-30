<script setup lang="ts">
import { ref, watch, computed, withDefaults, inject } from 'vue'
import { Card, GameData, Choice } from '@/types/Game'

export interface Props {
  card: Card
  resources?: number
  data: GameData
  rotated?: boolean
  currentChoice?: Choice
  side?: 'A' | 'B'
}

const props = withDefaults(defineProps<Props>(), { rotated: false })
const emit = defineEmits<{ choose: [string] }>()

const showBig = ref(false)
const bigRef = ref(null)
const cardRef = ref(null)

const emitter = inject('emitter')

emitter.on('flip', ({ cardId, callback }) => {
  if (cardId === props.card.id) {
    flipped.value = true
    setTimeout(() => {
      flipped.value = false
      callback()
    }, 300)
  }
})



const question = computed(() => {
  return props.data.game.players[props.data.playerId].question
})

const choose = () => {
  if (choice.value) {
    emit('choose', choice.value)
  }
}

const choice = computed(() => {
  if(props.currentChoice) { return props.currentChoice }
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

    let y = cardRect.y - bigRect.height

    if (y < 0) {
      y = cardRect.y + cardRect.height
    }

    big.style.left = `${x}px`
    big.style.top = `${y}px`
  }
})

const src = computed(() => {
  return `/images/cards/${props.card.def.code}${ props.side ? props.side.toLowerCase() : '' }.jpg`
})

const otherSide = computed(() => {
  if (props.side?.toLowerCase() === 'a') {
    return `/images/cards/${props.card.def.code}b.jpg`
  }

  return null
})

const flipped = ref(false)

</script>

<template>
  <div>
    <div ref="cardRef" class="card" :id="card.id" @mouseover="showBig = true" @mouseleave="showBig = false" :class="{ rotated }" @click="choose">
      <img class="front" :class="{flipped, active}" :src="src" />
      <img class="back" :class="{flipped}" v-if="otherSide" :src="otherSide" />
      <span v-if="resources" class="resources">{{resources}}</span>
    </div>
    <img ref="bigRef" class="big" :class="{ rotated }" v-if="showBig" :src="src" :anchor="card.id" />
  </div>
</template>

<style scoped lang="scss">
.card {
  width: $card-width;
  aspect-ratio: $card-aspect;
  line-height: 0;
  display: grid;
  position: relative;
  isolation: isolate;

  &.rotated {
    width: calc($card-width / $card-aspect);
    aspect-ratio: calc(1 / $card-aspect);
  }
}

.card img {
  width: 100%;
  border-radius: 5px;
  grid-area: 1 / 1;
}

.resources {
  z-index: 100;
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
  aspect-ratio: $card-aspect;
  border-radius: 20px;
  z-index: 100;

  &.rotated {
    width: calc($card-width * 4 / $card-aspect);
    aspect-ratio: calc(1 / $card-aspect);
  }
}

.active {
  border: 2px solid red;
  box-sizing: border-box;
}

.front {
  transform-style: preserve-3d;
  backface-visibility: hidden;
  transform: rotateY(0deg);
}

.back {
  transform-style: preserve-3d;
  backface-visibility: hidden;
  transform: rotateY(-180deg);
}

.front.flipped {
  animation: flip-back 0.3s linear;
  animation-fill-mode: forwards;
}

.back.flipped {
  animation: flip-front 0.3s linear;
  animation-fill-mode: forwards;
}

@keyframes flip-back {
  0% {
    opacity: 1;
    transform: rotateY(0deg);
  }

  49% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }

  100% {
    transform: rotateY(-180deg);
    opacity: 0;
  }
}

@keyframes flip-front {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }

  49% {
    opacity: 0;
  }

  50% {
    opacity: 1;
  }

  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }

}
</style>
