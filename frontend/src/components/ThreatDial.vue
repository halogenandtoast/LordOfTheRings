<script setup lang="ts">
defineProps<{ threat: number }>()
</script>

<template>
  <div class="threat" :class="{ danger: threat >= 40 }"><span>{{ threat }}</span></div>
</template>

<style scoped>
.threat {
  width: calc(var(--card-width) * 2);
  aspect-ratio: 75 / 38;
  background-image: url('/images/threat.png');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
  color: var(--threat-text);
  letter-spacing: 25px;
  text-indent: 25px;
  flex: none;
}
.threat span {
  background: linear-gradient(var(--threat-light), var(--threat-light), var(--threat-red), var(--threat-dark));
  background-clip: text;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}
.threat.danger span { background: linear-gradient(var(--threat-danger-light), var(--threat-red), var(--threat-dark)); background-clip: text; -webkit-background-clip: text; }
</style>
