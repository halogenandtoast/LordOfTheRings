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
</template>
