<script setup lang="ts">
import { watchEffect } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute } from 'vue-router'
import NavBar from './components/NavBar.vue'

const route = useRoute()
const { t } = useI18n()

watchEffect(() => {
  document.title = t((route.meta.title as string | undefined) ?? 'appName')
})
</script>

<template>
  <NavBar />
  <RouterView :key="$route.path" />
  <!-- whose the game and its content are, on every page, as Arkham has it -->
  <i18n-t keypath="footer.notice" tag="footer" scope="global">
    <template #game><a href="https://www.fantasyflightgames.com/en/products/the-lord-of-the-rings-the-card-game/" rel="noreferrer" target="_blank" tabindex="-1">{{ $t('about.gameName') }}</a></template>
    <template #ffg><a href="https://www.fantasyflightgames.com" rel="noreferrer" target="_blank" tabindex="-1">Fantasy Flight Games (FFG)</a></template>
    <template #mee><a href="https://www.middleearth.com" rel="noreferrer" target="_blank" tabindex="-1">Middle-earth Enterprises</a></template>
  </i18n-t>
</template>

<style scoped>
footer {
  position: fixed;
  bottom: 0;
  z-index: 100;
  width: 100%;
  padding: 1px 8px;
  font-size: 0.6rem;
  color: var(--title);
  background: var(--page-background);
  text-align: center;
  pointer-events: none;
}
footer a { color: inherit; pointer-events: auto; }
@media (width < 800px) and (orientation: portrait) {
  footer { display: none; }
}
</style>
