<script setup lang="ts">
// One piece of a log entry's sentence. An i18n part renders through <i18n-t>, so a
// translated sentence still carries its card chips.
import { computed } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import type { LogPart, LogRef } from '../gameLog'
import type { CardView } from '../types'

const props = defineProps<{ part: LogPart }>()

const overlay = useCardOverlay()
// drawn as the token on the board rather than as an icon
const tokens = ['progress', 'damage', 'resource']

const i18nKey = computed(() => (props.part.tag === 'LogI18n' ? props.part.contents[0] : ''))
const i18nVars = computed(() => (props.part.tag === 'LogI18n' ? props.part.contents[1] : {}))
const hasVars = computed(() => Object.keys(i18nVars.value).length > 0)
// a `count` variable picks the plural form, as the server's i18n does
const plural = computed(() => {
  const count = i18nVars.value['count']
  return count?.tag === 'LogNumber' ? count.contents : undefined
})

// What the overlay needs to draw the card: a prisoner face down, a quest's other side.
const preview = (ref: LogRef): CardView | null =>
  ref.cardCode
    ? ({
        code: ref.cardCode,
        name: ref.name,
        flipped: ref.kind === 'RefQuest' && ref.faceDown,
        facedown: ref.kind === 'RefHero' && ref.faceDown ? 'player' : undefined,
      } as CardView)
    : null

const show = (ref: LogRef, event: PointerEvent) => {
  const card = preview(ref)
  if (card) overlay.show(card, event.currentTarget as HTMLElement, false, event.pointerType === 'touch')
}
</script>

<template>
  <span v-if="part.tag === 'LogText'">{{ part.contents }}</span>

  <!-- an i18n part without variables is plain text: one nested in another's slot as a
       component would leave the outer placeholder unfilled -->
  <span v-else-if="part.tag === 'LogI18n' && !hasVars">{{ $t(i18nKey) }}</span>

  <i18n-t v-else-if="part.tag === 'LogI18n'" :keypath="i18nKey" :plural="plural" tag="span" scope="global">
    <template v-for="(value, name) in i18nVars" #[name] :key="name">
      <LogPart :part="value" />
    </template>
  </i18n-t>

  <span
    v-else-if="part.tag === 'LogRefPart'"
    class="log-ref"
    :class="`log-ref--${part.contents.kind}`"
    @pointerenter="(e: PointerEvent) => e.pointerType === 'mouse' && show(part.contents as LogRef, e)"
    @pointerleave="(e: PointerEvent) => e.pointerType === 'mouse' && overlay.hide()"
    @pointerup="(e: PointerEvent) => e.pointerType === 'touch' && show(part.contents as LogRef, e)"
    >{{ part.contents.name }}</span
  >

  <span v-else-if="part.tag === 'LogNumber'" class="log-num">{{ part.contents }}</span>

  <span
    v-else-if="part.tag === 'LogDelta'"
    class="log-delta"
    :class="part.contents < 0 ? 'log-delta--down' : 'log-delta--up'"
    >{{ part.contents < 0 ? '−' : '+' }}{{ Math.abs(part.contents) }}</span
  >

  <img
    v-else-if="part.tag === 'LogIcon' && tokens.includes(part.contents)"
    class="log-token"
    :src="`/images/tokens/${part.contents}.png`"
    :alt="part.contents"
  />

  <span
    v-else-if="part.tag === 'LogIcon'"
    class="log-icon"
    :class="`log-icon--${part.contents}`"
    role="img"
    :aria-label="part.contents"
    :style="{ '--icon': `url('/images/icons/game/${part.contents}.svg')` }"
  />

  <span v-else-if="part.tag === 'LogList'">
    <template v-for="(item, i) in part.contents" :key="i">
      <span v-if="i > 0">{{ i === part.contents.length - 1 ? $t('log.listLast') : $t('log.listSeparator') }}</span>
      <LogPart :part="item" />
    </template>
  </span>
</template>

<style scoped>
.log-ref {
  color: var(--text);
  font-weight: 600;
  cursor: help;
  border-bottom: 1px dotted var(--line-strong);
}
.log-ref--RefPlayer { color: var(--title); border-bottom: 0; cursor: default; }
.log-ref--RefHero, .log-ref--RefAlly { color: #a9cbe0; }
.log-ref--RefEnemy { color: #e9a08c; }
.log-ref--RefLocation { color: #b9cf8c; }
.log-ref--RefTreachery { color: #d3a6dd; }
.log-ref--RefQuest, .log-ref--RefScenario { color: var(--accent-bright); }

.log-num { font-variant-numeric: tabular-nums; font-weight: 600; }

.log-delta { font-variant-numeric: tabular-nums; padding: 0 3px; border-radius: 3px; }
.log-delta--up { color: #b9cf8c; background: rgb(124 125 59 / 0.25); }
.log-delta--down { color: #e9a08c; background: rgb(176 80 58 / 0.25); }

.log-icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  margin-left: 1px;
  vertical-align: -0.15em;
  background: currentColor;
  mask: var(--icon) center / contain no-repeat;
}
.log-icon--willpower { color: var(--willpower); }
.log-icon--threat { color: var(--threat-red); }
.log-icon--attack { color: var(--attack); }
.log-icon--defense { color: var(--defense); }
.log-token { width: 1.15em; height: 1.15em; margin-left: 2px; vertical-align: -0.25em; }
</style>
