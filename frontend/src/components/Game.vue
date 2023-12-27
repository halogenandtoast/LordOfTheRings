<script setup lang="ts">
import { Game } from '@/types/Game'
import Player from './Player.vue'

defineProps<{ game: Game }>()
</script>
<template>
  <Player v-for="player in game.players" :key="player.id" :player="player" />
</template>
