<script lang="ts" setup>
import { ref } from 'vue'
import { useRouter } from 'vue-router'
import { signUp } from '../auth'
import AuthForm from '../components/AuthForm.vue'

const router = useRouter()
const username = ref('')
const email = ref('')
const password = ref('')
const error = ref<string | null>(null)
const submitting = ref(false)

const submit = async () => {
  submitting.value = true
  error.value = null
  try {
    await signUp(username.value, email.value, password.value)
    await router.push('/')
  } catch (e) {
    error.value = e instanceof Error ? e.message : 'Could not create the account.'
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <AuthForm title="Create an account" submit-label="Sign up" :error="error" :submitting="submitting" @submit="submit">
    <input v-model="username" type="text" autocomplete="username" placeholder="Username" required />
    <input v-model="email" type="email" autocomplete="email" placeholder="Email" required />
    <input v-model="password" type="password" autocomplete="new-password" minlength="8" placeholder="Password (8 characters or more)" required />
    <template #links>
      Already have an account? <RouterLink to="/sign-in">Sign in</RouterLink>
    </template>
  </AuthForm>
</template>
