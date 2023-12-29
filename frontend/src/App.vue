<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import { useWebSocket } from '@vueuse/core'
import Game from './components/Game.vue'
import Log from './components/Log.vue'
import { Choice } from './types/Game'

const gameData = ref(null)
const gameLog = ref([])

const onError = (e: Event) => {
  console.log(e)
}

const { status, data, send, open, close } = useWebSocket(`ws://${location.host}/api/data`, { autoReconnect: true, onError })

watch(status, (newStatus) => {
  console.log(newStatus)
})

watch(data, (message) => {
  const json = JSON.parse(message)
  switch (json.tag) {
    case "Update":
      gameData.value = json.contents
      break
    case "Log":
      gameLog.value.push(json.message)
      break
  }
})

fetch('/api/data').then((res) => res.json().then((newData) => gameData.value = newData))

const reset = () => {
  gameLog.value = []
  fetch('/api/reset', { method: "POST" })
}

const question = computed(() => {
  const current = gameData.value
  if(!current) return null

  return current.game.players[current.playerId].question
})

const choose = (e: Choice) => {
  fetch('/api/data', { method: "POST"
    , headers: {
      "Content-Type": "application/json",
      // 'Content-Type': 'application/x-www-form-urlencoded',
    }
    , body: JSON.stringify({choice: e, player: gameData.value.playerId})
  })
}
</script>

<template>
  <div id="app">
    <Game v-if="gameData" :data="gameData" @reset="reset" :question="question" @choose="choose" />
    <Log :log="gameLog" />
  </div>
</template>

<style scoped lang="scss">
#app {
  display: grid;
  gap: $gap;
  background: #000 url('/images/background.jpg');
  background-size: auto 100%;
  background-position: center center;
  background-repeat: no-repeat;
  height: 100%;
  padding: calc($gap * 2);
  box-sizing: border-box;
  grid-template-columns: 1fr 250px;
}
</style>
