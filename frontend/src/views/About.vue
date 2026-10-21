<template>
  <div class="about">
    <section>
      <h2>{{ $t('about.copyrightTitle') }}</h2>
      <div class="panel">
        <i18n-t keypath="about.copyrightBody" tag="p" scope="global">
          <template #game><a href="https://www.fantasyflightgames.com/en/products/the-lord-of-the-rings-the-card-game/" rel="noreferrer" target="_blank">{{ $t('about.gameName') }}</a></template>
        </i18n-t>
        <p>{{ $t('about.copyrightTolkien') }}</p>
        <p>{{ $t('about.copyrightArtwork') }}</p>
      </div>
    </section>
    <section>
      <h2>{{ $t('about.projectTitle') }}</h2>
      <div class="panel">
        <i18n-t keypath="about.projectBody" tag="p" scope="global">
          <template #author><a href="https://github.com/halogenandtoast" rel="noreferrer" target="_blank">halogenandtoast</a></template>
          <template #source><a href="https://github.com/halogenandtoast/LordOfTheRings" rel="noreferrer" target="_blank">GitHub</a></template>
        </i18n-t>
      </div>
    </section>
  </div>
</template>

<style scoped>
.about {
  width: min(720px, 100% - 32px);
  margin: 24px auto;
  display: grid;
  gap: 20px;
}
section { display: grid; gap: 10px; }
h2 { margin: 0; color: var(--title); }
.panel {
  padding: 4px 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  line-height: 1.5;
}
a { color: var(--accent-bright); }
</style>
