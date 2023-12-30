<script setup lang="ts">
import { computed } from 'vue'
import { GameData, Choice } from '@/types/Game'
import Player from './Player.vue'
import Scenario from './Scenario.vue'

const props = defineProps<{ data: GameData }>()
const game = computed(() => props.data.game)
const emit = defineEmits<{ reset: [], choose: [Choice] }>()

</script>
<template>
  <div class="game">
    <div>
      <button @click="emit('reset')">Reset</button>
      <Scenario :data="data" :scenario="game.scenario" @choose="$emit('choose', $event)" />
    </div>
    <Player v-for="player in game.players" :key="player.id" :player="player" :data="data" @choose="$emit('choose', $event)" />
  </div>
</template>

<style scoped lang="scss">
.game {
  display: grid;
  gap: $gap;
  height: 100%;
  padding: calc($gap * 2);
  box-sizing: border-box;
}

button {
  display: block;
  height: fit-content;
  width: fit-content;
}
</style>
