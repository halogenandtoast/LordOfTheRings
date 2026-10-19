<script setup lang="ts">
defineProps<{ count: number; back: 'player' | 'encounter' }>()
</script>

<template>
  <div class="pile" :class="[back, { empty: count === 0 }]">
    <span class="pile-count">{{ count }}</span>
  </div>
</template>
