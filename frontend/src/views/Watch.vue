<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchSpectatableGames } from '../api'
import GameRow from '../components/GameRow.vue'
import type { GameSummary } from '../server'

const { t } = useI18n()
const games = ref<GameSummary[] | null>(null)
const error = ref<string | null>(null)

fetchSpectatableGames()
  .then((result) => (games.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))
</script>

<template>
  <div class="page-container">
    <div class="page-content watch">
      <h2 class="page-title">{{ $t('watch.title') }}</h2>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <div v-else-if="games?.length === 0" class="box"><p>{{ $t('watch.none') }}</p></div>
      <GameRow v-for="game in games ?? []" :key="game.id" :game="game" spectate />
    </div>
  </div>
</template>

<style scoped>
.watch { width: min(70vw, 100%); margin: 0 auto; }
@media (max-width: 768px) { .watch { width: 100%; } }
.box { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 14px; border-radius: 5px; }
.box.error { border-color: var(--danger); }
p { margin: 0; }
</style>
