<script setup lang="ts">
import { Game } from '@/types/Game'
import Player from './Player.vue'

defineProps<{ game: Game }>()
</script>
<template>
  <div class="game">
    <Player v-for="player in game.players" :key="player.id" :player="player" />
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
</style>
