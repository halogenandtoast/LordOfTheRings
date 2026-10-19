<script setup lang="ts">
import { ref, computed, watch, inject } from 'vue'
import { useWebSocket } from '@vueuse/core'
import Game from './components/Game.vue'
import Log from './components/Log.vue'
import { Choice } from './types/Game'

const uiLock = ref<boolean>(false)
const resultQueue = ref<any>([])

watch(uiLock, async () => {
  if(!uiLock.value) {
    const r = resultQueue.value.shift()
    if (r) {
      handleResult(r)
    }
  }
})

const gameData = ref(null)
const gameLog = ref([])

const onError = (e: Event) => {
  console.log(e)
}

const { status, data, send, open, close } = useWebSocket(`${location.protocol === 'https:' ? 'wss' : 'ws'}://${location.host}/api/data`, { autoReconnect: true, onError })

watch(data, (message) => {
  const json = JSON.parse(message)
  handleResult(json)
})

const emitter = inject('emitter')

const handleResult = (result) => {
  switch (result.tag) {
    case "Update":
      if (uiLock.value) {
        resultQueue.value.push(result)
      } else {
        gameData.value = result.contents
      }
      break
    case "FlipCard":
      if (uiLock.value) {
        resultQueue.value.push(result)
      } else {
        uiLock.value = true
        emitter?.emit('flip', { cardId: result.cardId, callback: () => { uiLock.value = false } })
      }
      break
    case "Log":
      gameLog.value.push(result.message)
      break
  }
}

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
  height: 100%;
  padding: calc($gap * 2);
  box-sizing: border-box;
  grid-template-columns: 1fr 280px;
}
</style>
