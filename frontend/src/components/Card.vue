<script setup lang="ts">
import { ref, computed, withDefaults, inject, onUnmounted } from 'vue'
import { useCardOverlay } from '@/composables/useCardOverlay'
import { Card, GameData, Choice } from '@/types/Game'

export interface Props {
  card: Card
  resources?: number
  progress?: number
  damage?: number
  data: GameData
  rotated?: boolean
  currentChoice?: Choice
  side?: 'A' | 'B'
  you: Player
}

const props = withDefaults(defineProps<Props>(), { rotated: false })
const emit = defineEmits<{ choose: [string] }>()

const cardRef = ref<HTMLElement | null>(null)
const overlay = useCardOverlay()

const emitter = inject('emitter')

const onFlip = ({ cardId, callback }) => {
  if (cardId === props.card.id) {
    flipped.value = true
    setTimeout(() => {
      flipped.value = false
      callback()
    }, 300)
  }
}

emitter.on('flip', onFlip)
onUnmounted(() => {
  emitter.off('flip', onFlip)
  overlay.hide()
})

const showOverlay = () => {
  if (cardRef.value) overlay.show(src.value, cardRef.value, props.rotated)
}

const question = computed(() => {
  return props.data.game.players[props.you.id].question
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
      return currentQuestion.contents.find((choice: Choice) => choice.tag === "TargetChoice" && choice.target.contents === props.card.id)
    default:
      return null
  }
})

const active = computed(() => {
  const currentChoice = choice.value
  return currentChoice !== undefined && currentChoice !== null
})

const src = computed(() => {
  return `/images/cards/${props.card.cardCode}${ props.side ? props.side.toLowerCase() : '' }.jpg`
})

const otherSide = computed(() => {
  if (props.side?.toLowerCase() === 'a') {
    return `/images/cards/${props.card.cardCode}b.jpg`
  }

  return null
})

const flipped = ref(false)

</script>

<template>
  <div>
    <div ref="cardRef" class="card" :id="card.id" @mouseenter="showOverlay" @mouseleave="overlay.hide()" :class="{ rotated }" @click="choose">
      <img class="front" :class="{flipped, active}" :src="src" />
      <img class="back" :class="{flipped}" v-if="otherSide" :src="otherSide" />
      <span v-if="resources" class="token resources">{{resources}}</span>
      <span v-if="progress" class="token progress">{{progress}}</span>
      <span v-if="damage" class="token damage">{{damage}}</span>
    </div>
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

.token {
  z-index: 100;
  display: grid;
  place-items: center;
  background-size: cover;
  width: calc($card-width / 4);
  height: calc($card-width / 4);
  color: #000;
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
  color: #fff;
  border-radius: 50%;
  border: 1px solid rgb(0 0 0 / 0.5);
}

.active {
  border: 2px solid var(--select);
  box-sizing: border-box;
}
.card:has(.active) { cursor: pointer; }

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
