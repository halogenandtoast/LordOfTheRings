<script setup lang="ts">
/**
 * The game's sphere and stat glyphs. Where the app has the real icon (from
 * lotr-lcg-assets) it is used, tinted like text; the simple drawn shapes cover
 * the rest. They read at 1em and carry the sphere colour.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { IconName } from '@browser/lib/card-text'

const { name, label } = defineProps<{
  name: IconName
  /** Announced to screen readers; omit for decorative use. */
  label?: string
}>()

const { t } = useI18n()

const title = computed(() => label ?? t(LABELS[name]))

const REAL_ICONS = new Set<IconName>([
  'leadership', 'lore', 'spirit', 'tactics', 'baggins', 'fellowship',
  'willpower', 'attack', 'defense', 'threat',
])
const realIcon = computed(() => REAL_ICONS.has(name) ? `url('/images/icons/game/${name}.svg')` : null)

const LABELS: Record<IconName, string> = {
  leadership: 'browser.spheres.leadership',
  lore: 'browser.spheres.lore',
  spirit: 'browser.spheres.spirit',
  tactics: 'browser.spheres.tactics',
  baggins: 'browser.spheres.baggins',
  fellowship: 'browser.spheres.fellowship',
  neutral: 'browser.spheres.neutral',
  willpower: 'browser.stats.willpower',
  attack: 'browser.stats.attack',
  defense: 'browser.stats.defense',
  threat: 'browser.stats.threat',
  'per-player': 'browser.icons.perPlayer',
}
</script>

<template>
  <span
    v-if="realIcon"
    class="icon game-icon"
    :class="`game-icon--${name}`"
    :style="{ '--icon': realIcon }"
    role="img"
    :aria-label="title"
    :title="title"
  />
  <svg
    v-else
    class="game-icon"
    :class="`game-icon--${name}`"
    viewBox="0 0 24 24"
    role="img"
    :aria-label="title"
    focusable="false"
  >
    <title>{{ title }}</title>

    <!-- Spheres: a crown, a leaf, a star, crossed blades, a ring, a tree. -->
    <template v-if="name === 'leadership'">
      <path d="M3 17 L5 7 L9.5 12 L12 5 L14.5 12 L19 7 L21 17 Z" />
      <rect x="3" y="18" width="18" height="2.5" rx="1" />
    </template>

    <template v-else-if="name === 'lore'">
      <path d="M12 3 C6 7 4 12 6 17 C9 21 15 21 18 17 C20 12 18 7 12 3 Z" />
      <path class="detail" d="M12 6 V19" />
    </template>

    <template v-else-if="name === 'spirit'">
      <path
        d="M12 2 L14.2 9 L21.5 9 L15.6 13.3 L17.8 20.5 L12 16 L6.2 20.5 L8.4 13.3 L2.5 9 L9.8 9 Z"
      />
    </template>

    <template v-else-if="name === 'tactics'">
      <path d="M4 3 L20 19 M20 3 L4 19" class="stroke" />
      <circle cx="12" cy="11" r="3" />
    </template>

    <template v-else-if="name === 'baggins'">
      <circle cx="12" cy="12" r="8" class="stroke" />
      <circle cx="12" cy="12" r="4" class="stroke" />
    </template>

    <template v-else-if="name === 'fellowship'">
      <path d="M12 21 C4 15 2 10 5 6 C8 3 11 5 12 7 C13 5 16 3 19 6 C22 10 20 15 12 21 Z" />
    </template>

    <template v-else-if="name === 'neutral'">
      <circle cx="12" cy="12" r="7" class="stroke" />
    </template>

    <!-- Stats. -->
    <template v-else-if="name === 'willpower'">
      <path d="M12 2 C9 7 6 9 6 13 A6 6 0 0 0 18 13 C18 9 15 7 12 2 Z" />
    </template>

    <template v-else-if="name === 'attack'">
      <path d="M12 2 L15 9 L15 16 L12 22 L9 16 L9 9 Z" />
      <rect x="6" y="14" width="12" height="2" rx="1" />
    </template>

    <template v-else-if="name === 'defense'">
      <path d="M12 2 L20 5 V12 C20 17 16 20.5 12 22 C8 20.5 4 17 4 12 V5 Z" />
    </template>

    <template v-else-if="name === 'threat'">
      <path d="M12 2 L21 20 H3 Z" />
      <path class="cutout" d="M12 8 L16.5 17 H7.5 Z" />
    </template>

    <template v-else-if="name === 'per-player'">
      <circle cx="8" cy="9" r="3" />
      <circle cx="16" cy="9" r="3" />
      <path d="M2.5 19 C3.5 14.5 12.5 14.5 13.5 19 Z" />
      <path d="M10.5 19 C11.5 14.5 20.5 14.5 21.5 19 Z" />
    </template>
  </svg>
</template>

<style scoped>
.game-icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.135em;
  fill: currentcolor;
  flex: none;
}

.stroke {
  fill: none;
  stroke: currentcolor;
  stroke-width: 2;
  stroke-linecap: round;
}

.detail {
  fill: none;
  stroke: var(--c-bg);
  stroke-width: 1.5;
  opacity: 0.7;
}

.cutout {
  fill: var(--c-bg);
}

/* Sphere glyphs carry their own colour; stat glyphs inherit the text colour. */
.game-icon--leadership {
  color: var(--c-leadership);
}
.game-icon--lore {
  color: var(--c-lore);
}
.game-icon--spirit {
  color: var(--c-spirit);
}
.game-icon--tactics {
  color: var(--c-tactics);
}
.game-icon--baggins {
  color: var(--c-baggins);
}
.game-icon--fellowship {
  color: var(--c-fellowship);
}
.game-icon--neutral {
  color: var(--c-neutral);
}
</style>
