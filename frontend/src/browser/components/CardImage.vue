<script setup lang="ts">
/**
 * A card's scan, at the right aspect ratio for its orientation.
 *
 * Reserves the box before the image loads so a scrolling grid never reflows,
 * and falls back to a titled placeholder for the handful of cards whose scan
 * is missing.
 */
import { computed, ref, watch } from 'vue'
import type { Card, CardImage } from '@browser/types/card'
import { imageUrlFor } from '@browser/types/card'

const {
  card,
  image,
  size = 'thumb',
  eager = false,
  tight = false,
} = defineProps<{
  card: Card
  /** Which of the card's images to show; defaults to the front face. */
  image?: CardImage
  size?: 'thumb' | 'full'
  eager?: boolean
  /** Crop for a very small box, where the centre of a card shows nothing. */
  tight?: boolean
}>()

const chosen = computed(() => image ?? card.images[0])
const src = computed(() => (chosen.value ? imageUrlFor(chosen.value, size) : null))
const orientation = computed(() => chosen.value?.orientation ?? card.orientation)

const failed = ref(false)
watch(src, () => {
  failed.value = false
})
</script>

<template>
  <div class="frame" :class="{ 'frame--tight': tight }" :data-orientation="orientation">
    <img
      v-if="src && !failed"
      :src="src"
      :alt="card.name"
      :loading="eager ? 'eager' : 'lazy'"
      :fetchpriority="eager ? 'high' : 'auto'"
      decoding="async"
      draggable="false"
      @error="failed = true"
    />
    <div v-else class="placeholder">
      <span class="placeholder__name">{{ card.name }}</span>
      <span v-if="card.number !== null" class="placeholder__number">
        {{ card.setName }} {{ String(card.number).padStart(3, '0') }}
      </span>
    </div>
  </div>
</template>

<style scoped>
.frame {
  position: relative;
  overflow: hidden;
  border-radius: var(--radius-card);
  background: var(--c-surface-3);
  aspect-ratio: var(--card-aspect-portrait);
}

.frame[data-orientation='landscape'] {
  aspect-ratio: var(--card-aspect-landscape);
}

img {
  width: 100%;
  height: 100%;
  object-fit: cover;
}

/*
 * At list-row size a card is 36px wide, too small to read. Anchoring the crop
 * high keeps the title band and the illustration's subject in frame instead of
 * centring on the rules text.
 */
.frame--tight img {
  object-position: 50% 18%;
}

.placeholder {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  gap: var(--space-1);
  height: 100%;
  padding: var(--space-3);
  border: 1px dashed var(--c-border-strong);
  border-radius: inherit;
  text-align: center;
}

.placeholder__name {
  font-family: var(--font-display);
  font-size: var(--text-sm);
  color: var(--c-text-muted);
  text-wrap: balance;
}

.placeholder__number {
  font-size: var(--text-xs);
  color: var(--c-text-faint);
}
</style>
