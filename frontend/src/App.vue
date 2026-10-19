<script setup lang="ts">
import NavBar from './components/NavBar.vue'
</script>

<template>
  <NavBar />
  <RouterView :key="$route.fullPath" />
</template>
