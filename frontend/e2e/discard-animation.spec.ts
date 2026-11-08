import { expect, test } from '@playwright/test'

for (const occupied of [false, true]) {
  test(`an event lands over the ${occupied ? 'previous discard' : 'empty placeholder'}`, async ({ page }) => {
    await page.goto('/')
    await page.evaluate(async (occupied) => {
      // @ts-expect-error Browser modules served by Vite.
      const { createApp, h, reactive, nextTick } = await import('/node_modules/.vite/deps/vue.js')
      // @ts-expect-error Browser modules served by Vite.
      const { createI18n } = await import('/node_modules/.vite/deps/vue-i18n.js')
      // @ts-expect-error Browser modules served by Vite.
      const { default: DiscardPile } = await import('/src/components/DiscardPile.vue')
      // @ts-expect-error Browser modules served by Vite.
      const { default: GameCard } = await import('/src/components/GameCard.vue')
      // @ts-expect-error Browser modules served by Vite.
      const { dealFrom } = await import('/src/composables/dealing.ts')
      const card = (id: string, code: string) => ({ id, code, name: id, type: 'Event', spheres: [], stats: [], counting: [], damage: 0 })
      const event = card('event', '01050')
      const previous = card('previous', '01051')
      const before = { started: true, players: [{ playerId: 'one', hand: [event], inPlay: [], discard: occupied ? [previous] : [], deckSize: 10 }] }
      const after = { started: true, players: [{ ...before.players[0], hand: [], discard: [event, ...before.players[0].discard] }] }
      const state = reactive({ view: before })
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      createApp({ render: () => h('div', { style: 'display:flex;gap:100px' }, [
        h('div', { class: 'hand' }, state.view.players[0].hand.map((card: any) => h(GameCard, { card, key: card.id }))),
        h(DiscardPile, { cards: state.view.players[0].discard, back: 'player', 'data-pile': 'discard-one' }),
      ]) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: {} }, missingWarn: false, fallbackWarn: false })).mount(host)
      await nextTick()
      const dealing = dealFrom(before, after)
      const transition = document.startViewTransition(async () => {
        dealing.lift()
        state.view = after
        await nextTick()
        dealing.land()
      })
      await transition.ready
      document.getAnimations().forEach(animation => animation.pause())
      ;(window as any).finishDiscard = async () => {
        document.getAnimations().forEach(animation => animation.finish())
        await transition.finished
        dealing.done()
      }
    }, occupied)
    const surface = page.locator('[data-pile-surface="discard-one"]')
    await expect(surface).toHaveCount(1)
    await expect(surface.locator('[data-card-id]')).toHaveCount(0)
    if (occupied) await expect(surface.locator('img')).toHaveAttribute('src', /01051/)
    else await expect(surface.locator('.empty')).toHaveCount(1)
    const snapshots = await page.evaluate(() => document.getAnimations().map(animation => (animation.effect as KeyframeEffect).pseudoElement))
    expect(snapshots).toContain('::view-transition-group(card-event)')
    expect(snapshots).not.toContain('::view-transition-group(card-previous)')
    await expect(page.locator('[data-pile="discard-one"] [data-card-id="event"]')).toHaveCount(1)
    await page.evaluate(() => (window as any).finishDiscard())
    await expect(surface).toHaveCount(0)
  })
}
