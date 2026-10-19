<script setup lang="ts">
defineProps<{ status: 'connecting' | 'open' | 'closed' }>()
</script>

<template>
  <header id="nav">
    <nav class="main-links">
      <a href="/" class="home-link">The Lord of the Rings</a>
    </nav>
    <div class="user-links">
      <span class="status" :data-status="status">{{ status }}</span>
    </div>
  </header>
</template>

<style scoped>
#nav {
  background: var(--background-dark);
  border-bottom: 1px solid rgb(255 255 255 / 0.07);
  box-shadow: 0 1px 12px rgb(0 0 0 / 0.5);
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

.home-link {
  display: flex;
  align-items: center;
  padding: 0 14px;
  color: color-mix(in srgb, var(--accent) 70%, #ccc);
  font-size: 0.925rem;
  font-weight: 700;
  text-decoration: none;
  white-space: nowrap;
  transition: color 0.15s;
}
.home-link:hover { color: color-mix(in srgb, var(--accent) 85%, white); }

.user-links {
  display: flex;
  align-items: center;
  padding: 0 16px;
}

.status {
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  opacity: 0.5;
}
.status[data-status='closed'] { color: var(--danger); opacity: 1; }
</style>
