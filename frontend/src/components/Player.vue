<script setup lang="ts">
import { ref, computed } from 'vue'
import { Player, GameData, Choice } from '@/types/Game'
import gsap from 'gsap'
import Card from './Card.vue'
import Ally from './Ally.vue'
const props = defineProps<{ you: Player, player: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()

const animationMatrix = ref([])

const nextId = ref(4)
const above = ref([])
const below = ref([{ id: 1, color: "#FF0000" }, { id: 2,  color: "#0000FF" }, { id: 3, color: "#00FF00" }])

const allies = computed(() => {
  return Object.values(props.data.game.entities.allies).filter((ally) => ally.placement.contents === props.player.id)
})

const heroes = computed(() => {
  return Object.values(props.data.game.entities.heroes).filter((hero) => hero.player === props.player.id)
})

function onBeforeEnter(el) {
  el.style.opacity = 0
  el.style.width = 0
}

function onEnter(el, done) {
  const index = el.dataset.index
  const finalRect = el.getBoundingClientRect()

  if(!index) {
    const width = window.getComputedStyle(el).width
    gsap.to(el, { opacity: 1, width, onComplete: done })
    return
  }

  const data = animationMatrix.value.find(([idx,]) => idx === index)
  console.log(data)
  animationMatrix.value = animationMatrix.value.filter(([idx,]) => idx !== index)

  if (!data) {
    gsap.to(el, { startAt: { height: 0, marginLeft: 50, marginTop: 70 }, ease: "power4.in", opacity: 1, width: 100, height: 140, marginLeft: 0, marginTop: 0, onComplete: done })
    return
  }

  const [,rect] = data

  const startX = rect.left - finalRect.left
  const startY = rect.top - finalRect.top

  const c = el.cloneNode(true)
  c.style.position = "fixed"
  c.style.width = rect.width + "px"
  c.style.zIndex = "1000"
  el.parentNode.insertBefore(c, el)

  const tl = gsap.timeline()

  tl.
    add("start").
    to(el, {
      startAt: { opacity: 0, width: 0 },
      width: rect.width,
      duration: 0.3
    }, "start").
    to(c, {
      startAt: { x: startX, y: startY, opacity: 1 },
      y: 0,
      x: 0,
      onComplete: () => {
        c.remove()
        el.style.opacity = "1"
        done()
      },
      duration: 0.3
    }, "start")
}

function onLeave(el, done) {
  animationMatrix.value = [...animationMatrix.value, [el.dataset.index, el.getBoundingClientRect()]]
  gsap.to(el, {
    startAt: { opacity: 0 },
    width: 0,
    margin: 0,
    onComplete: done,
    duration: 0.3
  })
}
</script>

<template>
  <div class="player">
    <div class="play-area">
      <transition-group @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter">
        <Ally v-for="ally in allies" :ally="ally" :key="ally.id" :you="you" :data="data" @choose="$emit('choose', $event)" />
      </transition-group>
    </div>
    <div class="heroes">
      <Card v-for="hero in heroes" :key="hero.id" :card="hero.card" :resources="hero.resources" :data="data" @choose="$emit('choose', $event)" :you="you" />
    </div>
    <div class="player-main">
      <div class="threat">
        <span>{{player.threat}}</span>
      </div>
      <div class="discard">
      </div>
      <div class="deck">
        <div class="deck-size">{{player.deck.length}}</div>
      </div>
      <div class="hand">
        <transition-group @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter">
          <Card v-for="card in player.hand" :key="card.id" :card="card" :data="data" @choose="$emit('choose', $event)" :data-index="card.id" :you="you" />
        </transition-group>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.player {
  align-self: flex-end;
  gap: $gap;
  display: flex;
  flex-direction: column;
}

.heroes {
  display: flex;
  gap: $gap;
}

.hand {
  display: flex;
  width: $card-width;
  flex: 1;
  gap: $gap;
}

.discard {
  height: $card-width;
  aspect-ratio: 1063/756;
}

.deck {
  width: $card-width;
  aspect-ratio: 756/1063;
  background-image: url('/images/player.jpg');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
}

.deck-size {
  color: white;
  background: rgba(0, 0, 0, 0.8);
  padding: calc($card-width / 10);
  border-radius: calc($card-width / 5);
  border: 1px solid white;
  font-size: $font-small;
}

.player-main {
  display: flex;
  flex-direction: row;
  gap: $gap;
  align-items: center;
}

.threat {
  width: calc($card-width * 2);
  aspect-ratio: 75/38;
  background-image: url('/images/threat.png');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
  color: rgba(255, 255, 255);
  letter-spacing: 25px;
  text-indent: 25px;
  span {
    background: -webkit-linear-gradient(#E8DDA6, #E8DDA6, #E93323, #822017);
    -webkit-background-clip: text;
    background-clip: text;
    -webkit-text-fill-color: transparent;
  }
}

.play-area {
  display: flex;
  flex-direction: row;
  gap: $gap;
  flex: 1;
}

</style>
