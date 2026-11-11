<script setup lang="ts">
/**
 * Report a bug. Someone with a GitHub account files it there themselves, as Arkham
 * does: the server uploads the game's export, and a new tab opens GitHub's new-issue
 * page with the report filled in. For someone without one, the server files it with the
 * site's account and the report is followed on the site, under Bugs on the bar. Which
 * one is a setting, asked here the first time. A native `<dialog>`, as `KeyHelp` is.
 */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { fileBug, fileBugReport } from '../api'
import { currentUser, updateSettings } from '../auth'
import { refreshBugs } from '../bugs'
import { showModal } from '../composables/useViewTransition'
import { useServerText } from '../i18n'
import PrimaryButton from './PrimaryButton.vue'

const { open, gameId, description: initialDescription = '' } = defineProps<{
  open: boolean
  gameId: string
  description?: string
}>()
const emit = defineEmits<{ close: [] }>()
const { t } = useI18n()
const tr = useServerText()

const dialog = ref<HTMLDialogElement | null>(null)
const title = ref('')
const description = ref('')
const blocking = ref(true)
const submitting = ref(false)
const failed = ref<string | null>(null)
// asking whether they have a GitHub account, filling in the report, or done
const step = ref<'ask' | 'form' | 'filed'>('form')
const filedId = ref<string | null>(null)
const onGithub = computed(() => currentUser.value?.hasGithub === true)

watch(
  () => open,
  (isOpen) => {
    const el = dialog.value
    if (!el) return
    if (isOpen && !el.open) {
      description.value = initialDescription
      blocking.value = true
      failed.value = null
      filedId.value = null
      step.value = currentUser.value?.hasGithub == null ? 'ask' : 'form'
      showModal(el)
    } else if (!isOpen && el.open) el.close()
  },
  { flush: 'post' },
)

const choose = async (hasGithub: boolean) => {
  failed.value = null
  try {
    await updateSettings({ hasGithub })
    step.value = 'form'
  } catch {
    failed.value = t('game.fileBug.choiceFailed')
  }
}

const blockingLine = () => `blocks my game: ${blocking.value ? 'yes' : 'no'}`

const fileOnGithub = async () => {
  // open the tab while the click still counts, so a popup blocker lets it through
  const tab = window.open('', '_blank')
  try {
    const { url } = await fileBug(gameId)
    const body = `${description.value}\n\n${blockingLine()}\ngame: ${window.location.href}\nfile: ${url}`
    const params = new URLSearchParams({
      labels: blocking.value ? 'bug,blocks game' : 'bug',
      title: title.value,
      body,
      assignee: 'halogenandtoast',
      projects: 'halogenandtoast/3',
    })
    const issue = `https://github.com/halogenandtoast/LordOfTheRings/issues/new?${params}`
    if (tab) tab.location.href = issue
    else window.open(issue, '_blank')
    title.value = ''
    description.value = ''
    emit('close')
  } catch {
    tab?.close()
    failed.value = t('game.fileBug.failed')
  }
}

const fileOnSite = async () => {
  try {
    const { id } = await fileBugReport(gameId, { title: title.value, description: description.value, blocking: blocking.value })
    filedId.value = id
    title.value = ''
    description.value = ''
    step.value = 'filed'
    void refreshBugs()
  } catch (error) {
    failed.value = tr(error instanceof Error ? error.message : '') || t('game.fileBug.siteFailed')
  }
}

const submit = async () => {
  submitting.value = true
  failed.value = null
  try {
    await (onGithub.value ? fileOnGithub() : fileOnSite())
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <dialog ref="dialog" class="file-bug" :aria-label="t('game.fileBug.title')" @cancel.prevent="emit('close')" @close="emit('close')">
    <div v-if="step === 'ask'" class="body">
      <h2>{{ t('game.fileBug.title') }}</h2>
      <p class="question">{{ t('game.fileBug.askGithub') }}</p>
      <div class="ways">
        <section>
          <h3>{{ t('game.fileBug.githubWay') }}</h3>
          <p>{{ t('game.fileBug.githubWayExplain') }}</p>
        </section>
        <section>
          <h3>{{ t('game.fileBug.siteWay') }}</h3>
          <p>{{ t('game.fileBug.siteWayExplain') }}</p>
        </section>
      </div>
      <p>{{ t('game.fileBug.changeLater') }}</p>
      <p v-if="failed" class="failed">{{ failed }}</p>
      <div class="actions">
        <button type="button" class="cancel" @click="emit('close')">{{ t('common.cancel') }}</button>
        <button type="button" class="secondary" @click="choose(false)">{{ t('game.fileBug.noGithub') }}</button>
        <PrimaryButton :label="t('game.fileBug.haveGithub')" @click="choose(true)" />
      </div>
    </div>
    <div v-else-if="step === 'filed'" class="body">
      <h2>{{ t('game.fileBug.filedTitle') }}</h2>
      <p>{{ t('game.fileBug.filedExplain') }}</p>
      <div class="actions">
        <button type="button" class="cancel" @click="emit('close')">{{ t('game.fileBug.backToGame') }}</button>
        <RouterLink v-if="filedId" :to="`/bugs/${filedId}`" class="follow" @click="emit('close')">{{ t('game.fileBug.follow') }}</RouterLink>
      </div>
    </div>
    <form v-else class="body" @submit.prevent="submit">
      <h2>{{ t('game.fileBug.title') }}</h2>
      <p>{{ onGithub ? t('game.fileBug.explain') : t('game.fileBug.explainSite') }}</p>
      <input v-model="title" type="text" required maxlength="200" :placeholder="t('game.fileBug.titlePlaceholder')" />
      <textarea v-model="description" required rows="6" maxlength="20000" :placeholder="t('game.fileBug.descriptionPlaceholder')"></textarea>
      <label class="check">
        <input v-model="blocking" type="checkbox" />
        {{ t('game.fileBug.blocking') }}
      </label>
      <p class="way">
        {{ onGithub ? t('game.fileBug.filingOnGithub') : t('game.fileBug.filingOnSite') }}
        <button type="button" class="link" @click="step = 'ask'">{{ t('game.fileBug.change') }}</button>
      </p>
      <p v-if="failed" class="failed">{{ failed }}</p>
      <div class="actions">
        <button type="button" class="cancel" @click="emit('close')">{{ t('common.cancel') }}</button>
        <PrimaryButton type="submit" :label="submitting ? t('game.fileBug.uploading') : t('game.fileBug.submit')" :disabled="submitting" />
      </div>
    </form>
  </dialog>
</template>

<style scoped>
.file-bug { max-width: none; max-height: none; padding: 0; border: 0; background: none; overflow: visible; }
.file-bug::backdrop { background: rgb(0 0 0 / 0.6); }

.body {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 16px;
  width: min(32rem, 100vw - 2rem);
  border: 1px solid var(--line-strong);
  border-radius: 5px;
  background: var(--panel);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
h2 { margin: 0; color: var(--title); font-size: 1.1rem; }
h3 { margin: 0 0 4px; color: var(--title); font-family: var(--font-body); font-size: 0.95rem; }
p { margin: 0; color: var(--text-muted); font-size: 0.875rem; }
.question { color: var(--text); }
.ways { display: grid; gap: 10px; grid-template-columns: repeat(auto-fit, minmax(13rem, 1fr)); }
.ways section { padding: 10px; border: 1px solid var(--line); border-radius: 4px; }
.check { display: flex; align-items: center; gap: 8px; font-size: 0.875rem; cursor: pointer; }
.check input { accent-color: var(--accent); }
.way { font-size: 0.8rem; }
.link { background: none; border: 0; padding: 0; color: var(--accent-bright); font: inherit; text-decoration: underline; cursor: pointer; }
.secondary { background: none; border: 1px solid var(--line-strong); border-radius: 3px; padding: 9px 14px; color: inherit; font: inherit; cursor: pointer; }
.follow { align-self: center; color: var(--accent-bright); font-weight: 600; }

input[type='text'], textarea {
  font: inherit;
  color: inherit;
  background: var(--input-background);
  border: 1px solid var(--line);
  border-radius: 3px;
  padding: 8px;
}
textarea { resize: vertical; }

.failed { color: var(--danger); }
.actions { display: flex; flex-wrap: wrap; justify-content: flex-end; align-items: center; gap: 10px; }
.cancel { background: none; border: 0; color: inherit; font: inherit; cursor: pointer; }
</style>
