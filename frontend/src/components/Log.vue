<script setup lang="ts">
const props = defineProps<{ log: string[] }>()
</script>

<template>
  <ul id="log">
    <li v-for="(entry, idx) in props.log" :key="idx">{{ entry }}</li>
  </ul>
</template>

<style scoped lang="scss">
#log {
  list-style: none;
  padding: 0;
  margin: 0;
  overflow-y: scroll;
  height: 100%;
  box-sizing: border-box;
  padding: calc($gap * 2);
  gap: $gap;
  display: flex;
  flex-direction: column-reverse;
  background: rgba(0, 0, 0, 0.3);
}

li {
    color: white;
}
</style>
