<script setup lang="ts">
// What can be done with a card that offers more than one choice, as Arkham's
// AbilitiesMenu: committing Faramir to the quest, or using his action.
import { nextTick, onMounted, ref, useId, useTemplateRef } from 'vue'
import { onClickOutside, onKeyStroke } from '@vueuse/core'
import { useServerText } from '../i18n'
import type { ChoiceView } from '../types'

const { options, anchor } = defineProps<{ options: ChoiceView[]; anchor: HTMLElement }>()
const emit = defineEmits<{ choose: [index: number]; close: [] }>()

const tr = useServerText()
const menu = useTemplateRef<HTMLElement>('menu')
const GAP = 8
const PAD = 8
const place = ref({ top: 0, left: 0 })
const tooltipId = useId()
const tooltip = useTemplateRef<HTMLElement>('tooltip')
const tooltipText = ref('')
const tooltipIndex = ref<number | null>(null)
const tooltipPlace = ref({ top: 0, left: 0 })
let tooltipRequest = 0

const hideTooltip = () => {
  tooltipRequest++
  tooltipText.value = ''
  tooltipIndex.value = null
}
const showTooltip = async (option: ChoiceView, event: Event) => {
  const button = event.currentTarget as HTMLElement
  hideTooltip()
  if (!option.tooltip) return
  const request = tooltipRequest
  tooltipText.value = tr(option.tooltip)
  tooltipIndex.value = option.index
  await nextTick()
  if (request !== tooltipRequest || !tooltip.value) return
  const rect = button.getBoundingClientRect()
  const box = tooltip.value.getBoundingClientRect()
  const menuRect = menu.value!.getBoundingClientRect()
  const below = menuRect.bottom + GAP
  tooltipPlace.value = {
    left: Math.max(PAD, Math.min(rect.left, window.innerWidth - box.width - PAD)),
    top: Math.max(PAD, below + box.height + PAD <= window.innerHeight ? below : menuRect.top - box.height - GAP),
  }
}

// beside the card, on its right unless that runs off the screen
onMounted(() => {
  const rect = anchor.getBoundingClientRect()
  const box = menu.value?.getBoundingClientRect()
  const width = box?.width ?? 200
  const height = box?.height ?? 0
  const left = rect.right + GAP + width + PAD > window.innerWidth ? rect.left - GAP - width : rect.right + GAP
  place.value = {
    top: Math.max(PAD, Math.min(rect.top, window.innerHeight - height - PAD)),
    left: Math.max(PAD, left),
  }
  menu.value?.querySelector('button')?.focus()
})

onClickOutside(menu, () => emit('close'), { ignore: [anchor] })
onKeyStroke('Escape', () => emit('close'))
</script>

<template>
  <Teleport to="body">
    <div ref="menu" class="choice-menu" role="menu" :style="{ top: `${place.top}px`, left: `${place.left}px` }">
      <button
        v-for="option in options" :key="option.index" type="button" role="menuitem"
        :aria-describedby="tooltipIndex === option.index ? tooltipId : undefined"
        @pointerenter="showTooltip(option, $event)" @pointerleave="hideTooltip"
        @focus="showTooltip(option, $event)" @blur="hideTooltip"
        @click="emit('choose', option.index)"
      >
        {{ tr(option.action) }}
      </button>
    </div>
    <div
      v-if="tooltipText" :id="tooltipId" ref="tooltip" role="tooltip" class="choice-tooltip"
      :style="{ top: `${tooltipPlace.top}px`, left: `${tooltipPlace.left}px` }"
    >{{ tooltipText }}</div>
  </Teleport>
</template>

<style scoped>
.choice-tooltip {
  position: fixed;
  z-index: 9001;
  width: max-content;
  max-width: min(19rem, calc(100vw - 16px));
  box-sizing: border-box;
  padding: 9px 12px;
  border: 1px solid var(--accent);
  border-radius: 6px;
  background: var(--panel);
  color: var(--text);
  font-size: 0.8rem;
  line-height: 1.5;
  box-shadow: 0 8px 24px var(--shadow-strong);
  pointer-events: none;
}
.choice-menu {
  position: fixed;
  z-index: 9000;
  display: flex;
  flex-direction: column;
  gap: 4px;
  min-width: 10rem;
  max-width: 18rem;
  padding: 4px;
  background: var(--panel);
  border: 1px solid var(--line-strong);
  border-radius: 6px;
  box-shadow: 0 8px 24px var(--shadow-strong);
}
.choice-menu button {
  padding: 6px 10px;
  font: inherit;
  font-size: 0.8rem;
  text-align: left;
  color: var(--text);
  background: rgb(229 211 173 / 0.06);
  border: 1px solid var(--line);
  border-radius: 4px;
  cursor: pointer;
}
.choice-menu button:hover,
.choice-menu button:focus-visible {
  color: var(--ink);
  background: var(--accent);
  border-color: var(--accent);
  outline: none;
}
</style>
