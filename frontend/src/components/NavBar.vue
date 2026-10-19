<template>
  <header id="nav">
    <nav class="main-links">
      <RouterLink to="/" class="home-link">The Lord of the Rings</RouterLink>
      <RouterLink to="/new-game" class="nav-link">New Game</RouterLink>
    </nav>
  </header>
</template>

<style scoped>
#nav {
  background: var(--nav-background);
  border-bottom: 1px solid var(--line);
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
</style>
