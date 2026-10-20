<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchAdminDashboard, type AdminDashboard } from '../api'
import GameRow from '../components/GameRow.vue'

const { t } = useI18n()
const dashboard = ref<AdminDashboard | null>(null)
const error = ref<string | null>(null)

fetchAdminDashboard()
  .then((result) => (dashboard.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))
</script>

<template>
  <div class="page-container">
    <div class="page-content admin">
      <h2>{{ $t('nav.admin') }}</h2>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <template v-if="dashboard">
        <div class="stats">
          <div class="box stat"><span class="value">{{ dashboard.users }}</span>{{ $t('admin.users') }}</div>
          <div class="box stat"><span class="value">{{ dashboard.activeUsers }}</span>{{ $t('admin.activeUsers') }}</div>
          <div class="box stat"><span class="value">{{ dashboard.games }}</span>{{ $t('admin.games') }}</div>
        </div>
        <h3>{{ $t('admin.recentGames') }}</h3>
        <!-- an admin may watch any game -->
        <GameRow v-for="game in dashboard.recentGames" :key="game.id" :game="game" spectate />
      </template>
    </div>
  </div>
</template>

<style scoped>
.page-container { flex: 1; min-height: 0; overflow-y: auto; padding: 20px 10px 10px; }
.admin { width: min(70vw, 100%); margin: 0 auto; }
@media (max-width: 768px) { .admin { width: 100%; } }
h2 { color: var(--title); font-size: 2em; text-transform: uppercase; letter-spacing: 0.04em; margin: 0 0 10px; }
h3 { color: var(--title); text-transform: uppercase; margin: 20px 0 10px; }
.box { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 14px; border-radius: 5px; }
.box.error { border-color: var(--danger); }
p { margin: 0; }
.stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 10px; }
.stat { display: flex; flex-direction: column; gap: 4px; color: var(--text-muted); font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.05em; }
.stat .value { color: var(--title); font-size: 2em; font-weight: 700; letter-spacing: 0; }
</style>
