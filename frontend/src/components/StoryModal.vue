<script setup lang="ts">
/**
 * A scenario's story from its rulebook (locales/en/story.json, by scenario slug): the
 * introduction with any special rules and tips before play, the resolution once won.
 * A native `<dialog>`, as `FileBug` is.
 */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import PrimaryButton from './PrimaryButton.vue'

export type StoryPart = 'intro' | 'resolution'

interface Story {
  intro?: string[]
  /** the introduction as the campaign tells it, in place of the scenario's own */
  campaignIntro?: string[]
  setup?: string[]
  /** the box's rules for its game modes, shown in a game played in that mode */
  easy?: string[]
  nightmare?: string[]
  rules?: { title: string; text: string }[]
  tips?: string[]
  resolution?: string[]
  /** the resolution as the campaign tells it, as the core set's with Mendor */
  campaignResolution?: string[]
}

const { scenario, part, campaign = false, mode = 'NormalMode' } = defineProps<{
  scenario: string
  name: string
  part: StoryPart | null
  campaign?: boolean
  mode?: string
}>()
const emit = defineEmits<{ close: [] }>()
const { t, tm, rt } = useI18n()

const story = computed(() => tm(`story.${scenario}`) as Story)
// each part a run of sections; a paragraph of `---` is the rulebook's divider
interface Section {
  heading?: string
  flavor?: boolean
  paragraphs?: string[]
  rules?: { title: string; text: string }[]
}
const sections = computed<Section[]>(() => {
  const s = story.value
  const text = (messages: string[] | undefined) => (messages ?? []).map((m) => rt(m))
  return part === 'intro'
    ? [
        { flavor: true, paragraphs: text((campaign && s.campaignIntro) || s.intro) },
        { heading: 'game.story.setup', paragraphs: text(s.setup) },
        { heading: 'game.story.rules', rules: (s.rules ?? []).map((r) => ({ title: rt(r.title), text: rt(r.text) })) },
        { heading: 'game.story.tips', paragraphs: text(s.tips) },
        { heading: 'game.story.easy', paragraphs: text(mode === 'EasyMode' ? s.easy : []) },
        { heading: 'game.story.nightmare', paragraphs: text(mode === 'NightmareMode' ? s.nightmare : []) },
      ]
    : [{ heading: 'game.story.resolution', flavor: true, paragraphs: text((campaign && s.campaignResolution) || s.resolution) }]
})

const dialog = ref<HTMLDialogElement | null>(null)
watch(
  () => part,
  (p) => {
    const el = dialog.value
    if (!el) return
    if (p && !el.open) el.showModal()
    else if (!p && el.open) el.close()
  },
  { flush: 'post' },
)
</script>

<template>
  <dialog ref="dialog" class="story" :aria-label="name" @cancel.prevent="emit('close')" @close="emit('close')">
    <article v-if="part">
      <h2>{{ name }}</h2>
      <template v-for="(section, i) in sections" :key="i">
        <template v-if="section.paragraphs?.length || section.rules?.length">
          <h3 v-if="section.heading">{{ t(section.heading) }}</h3>
          <template v-for="(p, j) in section.paragraphs" :key="j">
            <svg v-if="p === '---'" class="divider" viewBox="0 0 240 24" fill="none" stroke="currentColor" aria-hidden="true">
              <path d="M12 4v16M4 12h16M7 7l10 10M17 7 7 17" stroke-width="0.8" />
              <path d="M228 4v16M220 12h16M223 7l10 10M233 7l-10 10" stroke-width="0.8" />
              <path d="M24 10.5h80m-80 3h80M136 10.5h80m-80 3h80" stroke-width="0.9" />
              <path d="M88 12c4-6 10-6 12-1s-4 6-5 2M152 12c-4-6-10-6-12-1s4 6 5 2" stroke-width="1" />
              <circle cx="120" cy="12" r="6" stroke-width="1" />
              <circle cx="120" cy="12" r="2" fill="currentColor" stroke="none" />
            </svg>
            <p v-else :class="{ flavor: section.flavor }">{{ p }}</p>
          </template>
          <p v-for="(rule, j) in section.rules" :key="j"><strong>{{ rule.title }}.</strong> {{ rule.text }}</p>
        </template>
      </template>
      <div class="actions">
        <PrimaryButton :label="t('game.story.continue')" @click="emit('close')" />
      </div>
    </article>
  </dialog>
</template>

<style scoped>
/* the dialog is its own scrim, for the reason FileBug.vue gives */
.story {
  position: fixed;
  inset: 0;
  width: auto;
  height: auto;
  max-width: none;
  max-height: none;
  margin: 0;
  padding: 0;
  border: 0;
  background: rgb(0 0 0 / 0.6);
  view-transition-name: story;
}
.story[open] { display: grid; place-items: center; }

article {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 20px;
  width: min(36rem, 100vw - 2rem);
  max-height: calc(100dvh - 2rem);
  overflow-y: auto;
  border: 1px solid var(--line-strong);
  border-radius: 5px;
  background: var(--panel);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
h2 { margin: 0; color: var(--title); font-size: 1.2rem; }
h3 { margin: 8px 0 0; color: var(--title); font-size: 0.9rem; text-transform: uppercase; letter-spacing: 0.06em; }
p { margin: 0; font-size: 0.9rem; line-height: 1.5; white-space: pre-line; }
.flavor { font-style: italic; }
.divider { width: 12rem; height: 1.2rem; align-self: center; margin: 6px 0; color: var(--title); opacity: 0.8; }
.actions { display: flex; justify-content: flex-end; margin-top: 6px; }
</style>
