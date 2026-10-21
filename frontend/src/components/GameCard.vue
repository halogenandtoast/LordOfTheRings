<script setup lang="ts">
import { computed, reactive, ref, useTemplateRef, watch } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useDebug } from '../composables/useDebug'
import { useHighlights } from '../composables/useHighlights'
import { useServerText } from '../i18n'
import { cardDrag, cardImage, type CardView, type ChoiceView } from '../types'

const { card, choices = [], attachments = [], rotated = false, flipping = false, draggable = false, banner = false } = defineProps<{
  card: CardView
  choices?: ChoiceView[]
  attachments?: CardView[]
  rotated?: boolean
  flipping?: boolean
  /** Playable cards can also be dragged onto the board. */
  draggable?: boolean
  /** Name the card on its top strip, for an attachment peeking out of its host's stack. */
  banner?: boolean
}>()

const tr = useServerText()
const emit = defineEmits<{ choose: [index: number] }>()

const overlay = useCardOverlay()
const expanded = ref(false)
const cardRef = useTemplateRef<HTMLElement>('cardRef')

const source = computed(() => cardImage(card))
// the resource token takes the colour of the card's sphere, blending when it has several
const sphereTint = computed(() => {
  const colours = card.spheres.map((s) => `var(--${s.toLowerCase()})`)
  return colours.length > 1 ? `linear-gradient(135deg, ${colours.join(', ')})` : colours[0]
})
const choice = computed(() => choices.find((option) => option.targetId === card.id) ?? null)
const active = computed(() => choice.value !== null)
const highlights = useHighlights()
const isSource = computed(() => highlights.value.sourceId === card.id)
const isTarget = computed(() => highlights.value.targetIds.includes(card.id))
const isDrop = computed(() => highlights.value.dropIds.includes(card.id))
const choiceFor = (attached: CardView) =>
  choices.find((option) => option.targetId === attached.id) ?? null

// On a touch screen a long press reads a card, as does a tap on one with nothing to do.
const showOverlay = (touched = false) => cardRef.value && overlay.show(card, cardRef.value, rotated, touched)
let lastPointer = 'mouse'
let pressTimer: ReturnType<typeof setTimeout> | undefined
const cancelPress = () => clearTimeout(pressTimer)
const press = (event: PointerEvent) => {
  lastPointer = event.pointerType
  cancelPress()
  if (event.pointerType !== 'mouse') pressTimer = setTimeout(() => showOverlay(true), 400)
}

const choose = () => {
  if (choice.value) {
    emit('choose', choice.value.index)
  } else if (lastPointer !== 'mouse') {
    showOverlay(true)
  }
}

const dragStart = (event: DragEvent) => {
  overlay.hide()
  event.dataTransfer?.setData(cardDrag, card.id)
}

const { active: debugging, send } = useDebug()
const counts = computed(() => ({ Damage: card.damage, Progress: card.progress ?? 0, Resource: card.resources ?? 0 }))
const adjust = (token: keyof typeof counts.value, amount: 1 | -1) =>
  send({ tag: amount > 0 ? 'PlaceTokens' : 'RemoveTokens', contents: [{ tag: 'GameSource' }, card.target, token, 1] })

// Each time a token's count goes up it is placed again, keyed afresh so its drop plays.
const placed = reactive({ resources: 0, progress: 0, damage: 0 })
watch(
  () => [card.resources ?? 0, card.progress ?? 0, card.damage] as const,
  ([resources, progress, damage], [hadResources, hadProgress, hadDamage]) => {
    if (resources > hadResources) placed.resources++
    if (progress > hadProgress) placed.progress++
    if (damage > hadDamage) placed.damage++
  },
)

const toggleUnless = (attached: CardView) => {
  if (!choiceFor(attached)) {
    expanded.value = !expanded.value
  }
}

</script>

<template>
  <div class="card-slot" :class="{ expanded, hosting: attachments.length > 0, sideways: card.exhausted && !banner && !rotated, 'with-stats': card.stats.length && !banner && !card.exhausted }">
    <div v-if="attachments.length" class="attachments">
      <div
        v-for="(attached, index) in attachments"
        :key="attached.id"
        class="attached"
        :style="{ '--depth': attachments.length - index }"
        @click="toggleUnless(attached)"
      >
        <GameCard :card="attached" :choices="choices" banner @choose="emit('choose', $event)" />
      </div>
    </div>

    <div
      ref="cardRef"
      class="card"
      :class="{ rotated, exhausted: card.exhausted, active, playing: card.playing, source: isSource, target: isTarget, drop: isDrop, committed: card.committed, flipping }"
      :style="{ viewTransitionName: `card-${card.id}` }"
      :title="tr(card.name)"
      :data-card-id="card.id"
      :draggable="draggable && active && overlay.canHover.value"
      @dragstart="dragStart"
      @pointerenter="$event.pointerType === 'mouse' && showOverlay()"
      @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
      @pointerdown="press"
      @pointerup="cancelPress"
      @pointercancel="cancelPress"
      @contextmenu="lastPointer !== 'mouse' && $event.preventDefault()"
      @click="choose"
    >
      <img :src="source" :alt="tr(card.name)" />
      <!-- inside the card so it travels in the card's view transition snapshot -->
      <span v-if="banner" class="attached-name" :class="{ exhausted: card.exhausted }">{{ tr(card.name) }}</span>
      <span v-if="card.resources" :key="`resources-${placed.resources}`" class="token resources" :class="{ placed: placed.resources }" :style="{ '--sphere-tint': sphereTint }">{{ card.resources }}</span>
      <span v-if="card.progress" :key="`progress-${placed.progress}`" class="token progress" :class="{ placed: placed.progress }">{{ card.progress }}</span>
      <span v-if="card.damage > 0" :key="`damage-${placed.damage}`" class="token damage" :class="{ placed: placed.damage }">{{ card.damage }}</span>
    </div>
    <div v-if="card.stats.length && !banner" class="stats">
      <span v-for="[stat, value] in card.stats" :key="stat" :class="stat" :title="$t(`game.${stat}`, { value })">
        {{ value }}<span class="icon" :style="{ '--icon': `url('/images/icons/game/${stat}.svg')` }" />
      </span>
    </div>

    <div v-if="debugging && card.target" class="debug" @click.stop>
      <span v-for="(count, token) in counts" :key="token">
        <button type="button" :aria-label="$t('game.debug.removeToken', { token: $t(`game.debug.tokens.${token}`) })" :disabled="count === 0" @click="adjust(token, -1)">−</button>
        {{ $t(`game.debug.tokens.${token}`) }}
        <button type="button" :aria-label="$t('game.debug.addToken', { token: $t(`game.debug.tokens.${token}`) })" @click="adjust(token, 1)">+</button>
      </span>
    </div>
  </div>
</template>

<style scoped>
.card-slot { position: relative; line-height: 0; transition: margin 0.2s ease; }
/* under a card, its stats as modified: a strip off its bottom edge, one segment each */
.stats {
  --willpower: #8cc4ea;
  --attack: #f08a4b;
  --defense: #b9c4cf;
  display: flex;
  margin-top: 4px;
  border: 1px solid rgb(255 255 255 / 0.14);
  border-radius: 7px;
  background: rgb(20 16 10 / 0.92);
  overflow: hidden;
  font-size: 0.75rem;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  line-height: 1;
}
.stats > span { flex: 1; display: flex; justify-content: center; align-items: center; gap: 3px; padding: 3px 0; }
/* upright, the card squares off its bottom and drops its ring there, and the strip's
   border carries on its sides, so card and strip read as one with no line between */
.with-stats > .card img {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
  box-shadow: -1px 0 0 rgb(255 255 255 / 0.14), 1px 0 0 rgb(255 255 255 / 0.14), 0 -1px 0 rgb(255 255 255 / 0.14);
}
.with-stats > .stats { margin: 0 -1px; border-top: 0; border-radius: 0 0 7px 7px; }
.stats > span + span { border-left: 1px solid var(--line); }
.stats .threat { color: var(--threat-red); }
.stats .willpower { color: var(--willpower); }
.stats .attack { color: var(--attack); }
.stats .defense { color: var(--defense); }
/* an exhausted card turns past its own width (height × 0.88 scale), so make room for it */
.card-slot.sideways { margin-inline: calc((var(--card-width) / var(--card-aspect) * 0.88 - var(--card-width)) / 2); }
.card-slot.hosting { padding-top: calc(var(--attachment-peek) * v-bind('attachments.length')); }
.card-slot.hosting.expanded {
  padding-top: calc(var(--card-width) / var(--card-aspect) * v-bind('attachments.length') * 0.55);
}

.attachments {
  position: absolute;
  inset: 0 0 auto 0;
  display: grid;
  z-index: 1;
}
.attached {
  position: relative;
  grid-area: 1 / 1;
  cursor: pointer;
  transform: translateY(calc(var(--depth) * var(--attachment-peek) * -1 + var(--attachment-peek)));
  transition: transform 0.2s ease;
  filter: drop-shadow(0 -1px 4px var(--shadow));
}
/* The peeking strip of an attachment is its tiny title bar, so name it in readable text. */
.attached-name {
  position: absolute;
  inset: 0 0 auto 0;
  z-index: 3;
  height: var(--attachment-peek);
  display: flex;
  align-items: center;
  gap: 4px;
  padding: 0 6px;
  border-radius: 7px 7px 0 0;
  background: linear-gradient(rgb(20 16 10 / 0.92), rgb(20 16 10 / 0.75));
  color: var(--cream);
  font-size: 0.62rem;
  font-weight: 700;
  line-height: 1;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  pointer-events: none;
}
.attached-name.exhausted { color: var(--text-muted); }
.attached-name.exhausted::after { content: "· exhausted"; font-weight: 400; font-style: italic; }
/* Turning an attachment sideways would break the stack, so it dims instead. */
.attached :deep(.card.exhausted) { transform: none; }
.attached :deep(.card.exhausted img) { filter: brightness(0.6) saturate(0.6); }
.card-slot.expanded .attached {
  transform: translateY(calc(var(--depth) * (var(--card-width) / var(--card-aspect)) * -0.55
    + (var(--card-width) / var(--card-aspect)) * 0.55));
}

.card {
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  display: grid;
  position: relative;
  isolation: isolate;
  z-index: 2;
}
.card.rotated {
  width: calc(var(--card-width) / var(--card-aspect));
  aspect-ratio: calc(1 / 0.715);
}
.card img { width: 100%; border-radius: 7px; grid-area: 1 / 1; box-shadow: 0 0 0 1px rgb(255 255 255 / 0.14); }
/* a long press reads the card, not Safari's save-image menu */
.card { -webkit-touch-callout: none; -webkit-user-select: none; user-select: none; }
@media (hover: none) { .card img { -webkit-user-drag: none; } }
.card.exhausted { transform: rotate(90deg) scale(0.88); }
.card.committed img { box-shadow: 0 0 0 2px var(--accent); }
/* played from hand and being paid for: lifted out of the hand until it goes into play */
.card.playing { transform: translateY(-12%); }
.card.playing img { outline: 2px solid var(--accent); outline-offset: 2px; box-shadow: 0 6px 16px var(--shadow); }
.card.active { cursor: pointer; }
/* The visual language: magenta can be interacted with, cyan is the source of the
   decision, amber is its target. */
.card.active img { border: 1px solid var(--interactive); box-shadow: 0 0 10px var(--interactive-glow); }
.card.source img { outline: 2px solid var(--source); outline-offset: 2px; box-shadow: 0 0 12px var(--source-glow); }
.card.target img { outline: 2px solid var(--target); outline-offset: 2px; box-shadow: 0 0 12px var(--target-glow); }
.card.drop img { outline: 2px dashed var(--target); outline-offset: 2px; box-shadow: 0 0 12px var(--target-glow); }
.card.flipping { animation: flip 0.32s ease-in-out; }
.card.flipping.exhausted { animation: flip-exhausted 0.32s ease-in-out; }

@keyframes flip {
  0% { transform: rotateY(0deg); }
  50% { transform: rotateY(90deg) scale(1.04); }
  100% { transform: rotateY(0deg); }
}

@keyframes flip-exhausted {
  0% { transform: rotate(90deg) scale(0.88) rotateY(0deg); }
  50% { transform: rotate(90deg) scale(0.92) rotateY(90deg); }
  100% { transform: rotate(90deg) scale(0.88) rotateY(0deg); }
}

.token {
  z-index: 3;
  display: grid;
  place-items: center;
  background-size: cover;
  width: calc(var(--card-width) / 4);
  height: calc(var(--card-width) / 4);
  color: var(--token-text);
  font-weight: 700;
  font-size: 0.7rem;
  line-height: 1;
  grid-area: 1 / 1;
  /* lift the tokens off the card art */
  filter: drop-shadow(0 2px 2px rgb(0 0 0 / 0.6)) drop-shadow(0 0 1px rgb(0 0 0 / 0.8));
  text-shadow: 0 0 2px rgb(255 255 255 / 0.85);
}
.resources { background-image: url('/images/tokens/resource.png'); place-self: end end; position: relative; }
/* white, outlined in black, so it reads on any sphere's tint */
.token.resources {
  color: #fff;
  font-size: 0.85rem;
  text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000, 0 0 3px #000;
}
/* the tint colours the coin, under the number and inside the coin's outline */
.resources::before {
  content: '';
  position: absolute;
  inset: 0;
  z-index: -1;
  background: var(--sphere-tint, none);
  mask: url('/images/tokens/resource.png') center / cover;
  mix-blend-mode: color;
}
/* a token dropped onto the card */
@media (prefers-reduced-motion: no-preference) {
  .token.placed { animation: token-place 0.42s cubic-bezier(0.3, 1.4, 0.5, 1) both; }
}
@keyframes token-place {
  from { opacity: 0; translate: 0 -60%; scale: 1.6; }
  to { opacity: 1; translate: 0 0; scale: 1; }
}
.progress { background-image: url('/images/tokens/progress.png'); place-self: center; font-size: 1.1rem; }
.damage { background-image: url('/images/tokens/damage.png'); place-self: start end; color: var(--damage-text); text-shadow: 0 1px 2px rgb(0 0 0 / 0.9); }

/* debug mode: token adjusters over the bottom of a hovered card */
.debug {
  position: absolute;
  inset: auto 2px 2px 2px;
  z-index: 4;
  display: none;
  flex-direction: column;
  gap: 2px;
  padding: 3px;
  border-radius: 5px;
  background: rgb(20 16 10 / 0.88);
  color: var(--cream);
  font-size: 0.6rem;
  line-height: 1;
}
.card-slot:hover > .debug, .debug:focus-within { display: flex; }
.debug span { display: flex; align-items: center; justify-content: space-between; }
.debug button { width: 18px; height: 16px; padding: 0; border: 1px solid var(--line); border-radius: 3px; background: none; color: inherit; cursor: pointer; }
.debug button:disabled { opacity: 0.35; cursor: default; }
</style>
