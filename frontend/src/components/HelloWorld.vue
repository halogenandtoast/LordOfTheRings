<script setup lang="ts">
import { ref } from 'vue'
import Game from './Game.vue'

defineProps<{ msg: string }>()

const game = ref(null)

fetch('/api/data').then((res) => res.json().then((data) => game.value = data))

</script>

<template>
  <Game v-if="game" :game="game" />
</template>

<style scoped>
</style>
