<script setup lang="ts">
import { computed } from 'vue'
import { Game, Choice } from '@/types/Game'
import Player from './Player.vue'

const props = defineProps<{ data: GameData }>()
const game = computed(() => props.data.game)
const emit = defineEmits<{ reset: [], choose: [Choice] }>()

</script>
<template>
  <div class="game">
    <button @click="emit('reset')">Reset</button>
    <Player v-for="player in game.players" :key="player.id" :player="player" :data="data" @choose="$emit('choose', $event)" />
  </div>
</template>

<style scoped lang="scss">
.game {
  display: grid;
  gap: $gap;
  background: #000 url('/images/background.jpg');
  background-size: auto 100%;
  background-position: center center;
  background-repeat: no-repeat;
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
