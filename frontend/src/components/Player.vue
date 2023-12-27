<script setup lang="ts">
import { Player } from '@/types/Game'
import Card from './Card.vue'
defineProps<{ player: Player }>()
</script>

<template>
  <div class="heroes">
    <div v-for="hero in player.heroes" :key="hero.id"><Card :card="hero.card" /></div>
  </div>
  {{player.threat}}
</template>

<style scoped lang="scss">
.heroes {
  display: flex;
  gap: $gap;
}
</style>
