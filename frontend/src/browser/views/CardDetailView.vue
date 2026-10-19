<script setup lang="ts">
/**
 * A single card at its own URL, for linking and sharing.
 */
import '@browser/browser.css'
import { computed, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import CardDetail from '@browser/components/CardDetail.vue'
import { useCardStore } from '@browser/stores/cards'

const route = useRoute()
const store = useCardStore()

const code = computed(() => String(route.params.code ?? ''))
const card = computed(() => store.cardByCode(code.value))

onMounted(() => store.load())
</script>

<template>
  <main class="page card-browser">
    <RouterLink class="button button--ghost back" to="/cards">&#8592; Back to browsing</RouterLink>

    <p v-if="store.state === 'loading'" class="notice">Loading…</p>
    <p v-else-if="!card" class="notice">
      No card with the code <code>{{ code }}</code
      >.
    </p>
    <CardDetail v-else :card="card" />
  </main>
</template>

<style scoped>
.page {
  max-width: 60rem;
  margin-inline: auto;
  padding: var(--space-4) var(--space-4) var(--space-6);
}

.back {
  margin-bottom: var(--space-4);
}

.notice {
  padding: var(--space-6);
  color: var(--c-text-muted);
  text-align: center;
}

code {
  font-family: var(--font-mono);
}
</style>
