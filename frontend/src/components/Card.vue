<script setup lang="ts">
import { Card } from '@/types/Game'
defineProps<{ card: Card }>()
</script>

<template>
  <div class="card"><img :src="`/images/cards/${card.def.code}.jpg`" /></div>
</template>

<style scoped lang="scss">
.card {
  width: $card-width;
  height: fit-content;
  line-height: 0;
}

.card img {
  width: 100%;
  border-radius: 5px;
}
</style>
