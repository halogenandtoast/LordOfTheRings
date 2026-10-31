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
  rules?: { title: string; text: string }[]
  tips?: string[]
  resolution?: string[]
  /** a resolution only the campaign tells, as the core set's, with Mendor */
  campaignResolution?: string[]
}

const { scenario, part, campaign = false } = defineProps<{ scenario: string; name: string; part: StoryPart | null; campaign?: boolean }>()
const emit = defineEmits<{ close: [] }>()
const { t, tm, rt } = useI18n()

const story = computed(() => tm(`story.${scenario}`) as Story)
const resolution = computed(() => (campaign && story.value.campaignResolution) || story.value.resolution)
const text = (messages: string[] | undefined) => (messages ?? []).map((m) => rt(m))

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
      <template v-if="part === 'intro'">
        <p v-for="(p, i) in text(story.intro)" :key="i" class="flavor">{{ p }}</p>
        <template v-if="story.rules?.length">
          <h3>{{ t('game.story.rules') }}</h3>
          <p v-for="(rule, i) in story.rules" :key="i"><strong>{{ rt(rule.title) }}.</strong> {{ rt(rule.text) }}</p>
        </template>
        <template v-if="story.tips?.length">
          <h3>{{ t('game.story.tips') }}</h3>
          <p v-for="(p, i) in text(story.tips)" :key="i">{{ p }}</p>
        </template>
      </template>
      <template v-else>
        <h3>{{ t('game.story.resolution') }}</h3>
        <p v-for="(p, i) in text(resolution)" :key="i" class="flavor">{{ p }}</p>
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
.actions { display: flex; justify-content: flex-end; margin-top: 6px; }
</style>
