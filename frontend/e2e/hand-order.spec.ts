import { expect, test } from '@playwright/test'
import { moveHandCard, reconcileHandOrder } from '../src/handOrder'

test('rendered hand preserves reordering through updates, remounts, and seat changes', async ({ page }) => {
  await page.goto('/')
  const mount = async () => page.evaluate(async () => {
    // Load the real component through Vite, without requiring a game-server account.
    // @ts-expect-error Vite serves these browser modules.
    const { createApp, h, reactive } = await import('/node_modules/.vite/deps/vue.js')
    // @ts-expect-error Vite serves these browser modules.
    const { createI18n } = await import('/node_modules/.vite/deps/vue-i18n.js')
    // @ts-expect-error Vite compiles the SFC for the browser.
    const { default: PlayerArea } = await import('/src/components/PlayerArea.vue')
    const card = (id: string) => ({ id, code: '01013', name: id, type: 'Ally', spheres: [], stats: [], counting: [], damage: 0 })
    const state = reactive({ seat: 'one', ids: ['a', 'b', 'c'] })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    const app = createApp({ render: () => h(PlayerArea, {
      key: state.seat, gameId: 'hand-test',
      player: { playerId: state.seat, hand: state.ids.map(card), discard: [], onDeck: [], deckSize: 0, threat: 20 },
      choices: [], flipping: new Set(), question: null, outcome: null, victoryPoints: 0,
    }) })
    app.use(createI18n({ legacy: false, locale: 'en', missingWarn: false, fallbackWarn: false, messages: { en: {} } }))
    app.mount(host)
    ;(window as any).handState = state
  })
  await mount()
  const ids = () => page.locator('.hand [data-card-id]').evaluateAll(cards => cards.map(card => card.getAttribute('data-card-id')))
  const last = page.locator('.hand [data-card-id="c"]')
  await expect(last).toHaveAttribute('draggable', 'true')
  await page.locator('.hand').evaluate(hand => {
    hand.addEventListener('dragstart', event => { (window as any).allowedDragEffect = (event as DragEvent).dataTransfer?.effectAllowed })
    hand.addEventListener('dragover', event => { (window as any).handDropEffect = (event as DragEvent).dataTransfer?.dropEffect })
  })
  const first = await page.locator('.hand > .card-slot').first().boundingBox()
  const source = await last.boundingBox()
  await page.mouse.move(source!.x + source!.width / 2, source!.y + source!.height / 2)
  await page.mouse.down()
  await page.mouse.move(first!.x + 1, first!.y + 30, { steps: 10 })
  await expect.poll(ids).toEqual(['c', 'a', 'b'])
  expect(await page.evaluate(() => (window as any).allowedDragEffect)).toBe('move')
  expect(await page.evaluate(() => (window as any).handDropEffect)).toBe('move')
  // Staying in the same slot while cards animate must not make them swap back.
  await page.mouse.move(first!.x + 2, first!.y + 30)
  await expect.poll(ids).toEqual(['c', 'a', 'b'])
  await page.mouse.up()
  await expect.poll(ids).toEqual(['c', 'a', 'b'])
  await page.evaluate(() => { (window as any).handState.ids = ['d', 'a', 'c'] })
  await expect.poll(ids).toEqual(['c', 'a', 'd'])
  await page.evaluate(() => { (window as any).handState.seat = 'two' })
  await expect.poll(ids).toEqual(['d', 'a', 'c'])
  await page.evaluate(() => { (window as any).handState.seat = 'one' })
  await expect.poll(ids).toEqual(['c', 'a', 'd'])
  await page.reload()
  await mount()
  await expect.poll(ids).toEqual(['c', 'a', 'b'])
  await page.locator('.hand > .card-slot').first().press('Alt+ArrowRight')
  await expect.poll(ids).toEqual(['a', 'c', 'b'])
})

test('keeps the chosen order after playing a card and appends draws on the right', () => {
  const reordered = moveHandCard(['a', 'b', 'c'], 'c', 'a')
  expect(reordered).toEqual(['c', 'a', 'b'])
  expect(reconcileHandOrder(reordered, ['d', 'e', 'a', 'c'])).toEqual(['c', 'a', 'd', 'e'])
})

test('puts returning cards on the right and preserves a restored order', () => {
  const played = reconcileHandOrder(['c', 'a', 'b'], ['a', 'b'])
  expect(reconcileHandOrder(played, ['c', 'b', 'a'])).toEqual(['a', 'b', 'c'])
  expect(reconcileHandOrder(JSON.parse(JSON.stringify(['c', 'a', 'b'])), ['a', 'b', 'c'])).toEqual(['c', 'a', 'b'])
})

test('moves cards in either direction and ignores stale drag targets', () => {
  expect(moveHandCard(['a', 'b', 'c'], 'a', null)).toEqual(['b', 'c', 'a'])
  expect(moveHandCard(['a', 'b', 'c'], 'a', 'c')).toEqual(['b', 'a', 'c'])
  expect(moveHandCard(['a', 'b'], 'removed', 'a')).toEqual(['a', 'b'])
  expect(moveHandCard(['a', 'b'], 'a', 'removed')).toEqual(['a', 'b'])
})
