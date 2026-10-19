<script setup lang="ts">
import { useRouter } from 'vue-router'
import { currentUser, signOut } from '../auth'
import GameMenu from './GameMenu.vue'

const router = useRouter()

const leave = async () => {
  signOut()
  await router.push('/sign-in')
}
</script>

<template>
  <header id="nav">
    <nav class="main-links">
      <RouterLink to="/" class="home-link">{{ $t('appName') }}</RouterLink>
      <RouterLink v-if="currentUser" to="/" class="nav-link">{{ $t('nav.home') }}</RouterLink>
      <RouterLink to="/cards" class="nav-link">{{ $t('nav.cards') }}</RouterLink>
      <RouterLink v-if="currentUser" to="/decks" class="nav-link">{{ $t('nav.decks') }}</RouterLink>
    </nav>
    <div class="user-links">
      <GameMenu v-if="currentUser" class="user-menu" align="right">
        {{ currentUser.username }}
        <template #items>
          <button type="button" @click="leave">{{ $t('auth.signOut') }}</button>
        </template>
      </GameMenu>
      <template v-else>
        <RouterLink to="/sign-in">{{ $t('auth.signIn') }}</RouterLink>
        <RouterLink to="/sign-up">{{ $t('auth.signUp') }}</RouterLink>
      </template>
    </div>
  </header>
</template>

<style scoped>
#nav {
  background: var(--nav-background);
  border-bottom: 1px solid rgb(0 0 0 / 0.85);
  box-shadow: 0 1px 12px var(--shadow);
  display: flex;
  align-items: stretch;
  height: var(--nav-height);
  flex-shrink: 0;
  position: relative;
  z-index: 100;
}

.main-links {
  display: flex;
  align-items: stretch;
  flex: 1;
}

.home-link,
.nav-link {
  display: flex;
  align-items: center;
  padding: 0 14px;
  color: var(--nav-link);
  font-size: 0.875rem;
  font-weight: 500;
  text-decoration: none;
  position: relative;
  transition: color 0.15s;
  white-space: nowrap;
}
.nav-link:hover { color: var(--accent); }
.nav-link.router-link-active { color: var(--accent); }
.nav-link.router-link-active::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 8px;
  right: 8px;
  height: 2px;
  background: var(--accent);
  border-radius: 2px 2px 0 0;
}

.home-link {
  font-weight: 700;
  font-size: 0.925rem;
  color: var(--title);
}
.home-link:hover { color: var(--accent-bright); }

@media (max-width: 768px) {
  .nav-link { display: none; }
}

.user-links {
  display: flex;
  align-items: center;
  height: 100%;
  gap: 14px;
  padding: 0 16px;
  font-size: 0.875rem;
}
.user-links a {
  color: var(--nav-link);
  text-decoration: none;
  font-weight: 500;
}
.user-links a:hover { color: var(--accent); }

.user-menu :deep(.menu-items button) {
  background: none;
  border: 0;
  color: var(--text);
  font: inherit;
  padding: 10px 14px;
  text-align: left;
  cursor: pointer;
}
.user-menu :deep(.menu-items button:hover) { background: var(--hover-tint); }
</style>
