<script setup lang="ts">
defineProps<{ threat: number }>()
</script>

<template>
  <div class="threat" :class="{ danger: threat >= 40 }"><span>{{ threat }}</span></div>
</template>

<style scoped>
.threat {
  width: calc(var(--card-width) * 1.7);
  aspect-ratio: 75 / 38;
  background-image: url('/images/threat.png');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
  font-size: 1.5rem;
  font-weight: 700;
  letter-spacing: 0.12em;
  text-indent: 0.12em;
  flex: none;
}
.threat span {
  background: linear-gradient(#e8dda6, #e8dda6, #e93323, #822017);
  background-clip: text;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}
.threat.danger span { background: linear-gradient(#f3b0a4, #e93323, #822017); background-clip: text; -webkit-background-clip: text; }
</style>
