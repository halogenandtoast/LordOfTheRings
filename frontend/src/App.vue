<script setup lang="ts">
import { ref, computed } from 'vue'
import Game from './components/Game.vue'
import { Choice } from './types/Game'

const data = ref(null)

fetch('/api/data').then((res) => res.json().then((newData) => data.value = newData))

const reset = () => {
  fetch('/api/reset', { method: "POST" }).then((res) => res.json().then((newData) => data.value = newData))
}

const question = computed(() => {
  const current = data.value
  if(!current) return null

  return current.game.players[current.playerId].question
})

const choose = (e: Choice) => {
  fetch('/api/data', { method: "POST"
    , headers: {
      "Content-Type": "application/json",
      // 'Content-Type': 'application/x-www-form-urlencoded',
    }
    , body: JSON.stringify({choice: e, player: data.value.playerId})
  }).then((res) => res.json().then((newData) => data.value = newData))
}
</script>

<template>
  <Game v-if="data" :data="data" @reset="reset" :question="question" @choose="choose" />
</template>

<style scoped lang="scss">
</style>
