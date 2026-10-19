<script lang="ts" setup>
import { ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { signIn } from '../auth'
import AuthForm from '../components/AuthForm.vue'

const route = useRoute()
const router = useRouter()
const login = ref('')
const password = ref('')
const error = ref<string | null>(null)
const submitting = ref(false)

const submit = async () => {
  submitting.value = true
  error.value = null
  try {
    await signIn(login.value, password.value)
    await router.push(typeof route.query.next === 'string' ? route.query.next : '/')
  } catch (e) {
    error.value = e instanceof Error ? e.message : 'Could not sign in.'
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <AuthForm title="Sign in" submit-label="Sign in" :error="error" :submitting="submitting" @submit="submit">
    <input v-model="login" type="text" autocomplete="username" placeholder="Username or email" required />
    <input v-model="password" type="password" autocomplete="current-password" placeholder="Password" required />
    <template #links>
      New to Middle-earth? <RouterLink to="/sign-up">Create an account</RouterLink>
    </template>
  </AuthForm>
</template>
