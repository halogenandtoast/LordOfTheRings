<template>
  <div class="page-container">
    <div class="about">
      <h2 class="page-title">{{ $t('nav.about') }}</h2>
      <section>
        <h3>{{ $t('about.copyrightTitle') }}</h3>
        <div class="panel">
          <i18n-t keypath="about.copyrightBody" tag="p" scope="global">
            <template #game><a href="https://www.fantasyflightgames.com/en/products/the-lord-of-the-rings-the-card-game/" rel="noreferrer" target="_blank">{{ $t('about.gameName') }}</a></template>
          </i18n-t>
          <p>{{ $t('about.copyrightTolkien') }}</p>
          <p>{{ $t('about.copyrightArtwork') }}</p>
        </div>
      </section>
      <section>
        <h3>{{ $t('about.projectTitle') }}</h3>
        <div class="panel">
          <i18n-t keypath="about.projectBody" tag="p" scope="global">
            <template #author><a href="https://github.com/halogenandtoast" rel="noreferrer" target="_blank">halogenandtoast</a></template>
            <template #source><a href="https://github.com/halogenandtoast/LordOfTheRings" rel="noreferrer" target="_blank">GitHub</a></template>
          </i18n-t>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
.about {
  width: min(720px, 100% - 32px);
  margin: 0 auto;
  display: grid;
  gap: 12px;
}
/* the grid's gap is the room below it */
.about .page-title { margin: 0; }
section { display: grid; gap: 10px; }
h3 { margin: 0; color: var(--title); }
.panel {
  padding: 4px 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  line-height: 1.5;
}
a { color: var(--accent-bright); }
</style>
