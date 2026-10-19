<script lang="ts" setup>
import { useServerText } from '../i18n'

const tr = useServerText()
defineProps<{ title: string; error: string | null; submitting: boolean; submitLabel: string }>()
defineEmits<{ submit: [] }>()
</script>

<template>
  <form class="auth-form" @submit.prevent="$emit('submit')">
    <header>
      <img src="/icon.svg" alt="" class="logo" />
      <h2>{{ title }}</h2>
    </header>
    <div v-if="error" class="error" role="alert">{{ tr(error) }}</div>
    <section>
      <slot></slot>
      <button type="submit" :disabled="submitting">{{ submitting ? $t('auth.oneMoment') : submitLabel }}</button>
    </section>
    <section class="links">
      <slot name="links"></slot>
    </section>
  </form>
</template>

<style scoped>
.auth-form {
  margin: 10vh auto 0;
  width: 50vw;
  max-width: 400px;
  min-width: min(320px, 92vw);
}

header {
  text-align: center;
  margin-bottom: 10px;
}

.logo {
  display: block;
  width: 72px;
  height: 72px;
  margin: 0 auto;
}

h2 {
  color: var(--title);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  margin: 6px 0 0;
}

section {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 14px;
  display: grid;
  gap: 10px;
}

section.links {
  background: none;
  border: 0;
  text-align: center;
  color: var(--text-muted);
  font-size: 0.9em;
}

.links :deep(a) { color: var(--accent-bright); }

section :deep(input) {
  font: inherit;
  color: var(--text);
  outline: 0;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  padding: 14px;
  background: var(--input-background);
  width: 100%;
}
section :deep(input:focus) { border-color: var(--accent); }
section :deep(input::placeholder) { color: var(--text-muted); opacity: 0.7; }

button {
  font: inherit;
  font-weight: 700;
  outline: 0;
  padding: 14px;
  background: var(--button-primary);
  color: var(--button-primary-text);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  border: 0;
  border-radius: 3px;
  width: 100%;
  cursor: pointer;
}
button:hover { background: var(--button-primary-hover); }
button:disabled { background: var(--button-disabled); color: var(--button-disabled-text); cursor: wait; }

.error {
  background: var(--danger);
  color: var(--text);
  border-radius: 5px;
  margin: 10px 0;
  padding: 8px 12px;
}
</style>
