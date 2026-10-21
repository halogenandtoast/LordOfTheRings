<script setup lang="ts">
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { loadLocaleMessages, normalizeLocale, supportedLocales } from '../locales/messages'
import { table, tables } from '../table'

// As in Arkham: the language is kept in localStorage, and a language's messages are
// loaded the first time it is chosen, with no reload.
const { locale, availableLocales, setLocaleMessage } = useI18n({ useScope: 'global' })
const language = ref(normalizeLocale(locale.value))

// each language named in itself, so a player can find their own
const languageName = (code: string) => new Intl.DisplayNames([code], { type: 'language' }).of(code) ?? code

const updateLanguage = async () => {
  const chosen = normalizeLocale(language.value)
  if (!availableLocales.includes(chosen)) {
    const loaded = await loadLocaleMessages(chosen)
    setLocaleMessage(loaded.locale, loaded.messages)
  }
  locale.value = chosen
  localStorage.setItem('language', chosen)
  document.documentElement.lang = chosen
}
</script>

<template>
  <div class="settings">
    <h2>{{ $t('settings.title') }}</h2>
    <section class="panel">
      <label>
        {{ $t('settings.language') }}
        <select v-model="language" @change="updateLanguage">
          <option v-for="code in supportedLocales" :key="code" :value="code">{{ languageName(code) }}</option>
        </select>
      </label>
      <p class="hint">{{ $t('settings.languageHint') }}</p>
    </section>
    <section class="panel">
      <label>
        {{ $t('settings.table') }}
        <select v-model="table">
          <option v-for="option in tables" :key="option" :value="option">{{ $t(`settings.tables.${option}`) }}</option>
        </select>
      </label>
      <p class="hint">{{ $t('settings.tableHint') }}</p>
    </section>
  </div>
</template>

<style scoped>
.settings {
  width: min(560px, 100% - 32px);
  margin: 24px auto;
  display: grid;
  gap: 14px;
}
h2 { margin: 0; color: var(--title); }
.panel {
  display: grid;
  gap: 8px;
  padding: 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
}
label { display: grid; gap: 6px; font-size: 0.85rem; color: var(--text-muted); }
select {
  font: inherit;
  font-size: 1rem;
  color: var(--text);
  padding: 10px 2.25rem 10px 10px;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  background-color: var(--input-background);
}
select:focus { outline: 0; border-color: var(--accent); }
.hint { margin: 0; font-size: 0.8rem; color: var(--text-muted); }
</style>
