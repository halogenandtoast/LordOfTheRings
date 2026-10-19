import { expect, test as base, type APIRequestContext, type Page } from '@playwright/test'

// Each test signs up its own player and deals a fresh game from a fixed seed, then
// changes the game through the debug endpoint and watches the board respond.

interface Game {
  id: number
  playerId: string
  /** Run an engine message, as debug mode does. */
  debug: (message: object) => Promise<void>
  /** The game as the server sends it. */
  state: () => Promise<any>
  /** Answer the waiting question. */
  answer: (choice: number) => Promise<void>
}

const test = base.extend<{ game: Game }>({
  game: async ({ page, request }, use) => {
    const name = `e2e-${Date.now()}-${Math.floor(Math.random() * 1e6)}`
    const session = await ok(request.post('/api/register', { data: { username: name, email: `${name}@example.com`, password: 'e2e-password' } }))
    const headers = { Authorization: `Token ${session.token}` }
    const { id } = await ok(request.post('/api/games', { headers, data: { name, seed: 7 } }))
    const state = () => ok(request.get(`/api/games/${id}`, { headers }))
    const { playerId } = await state()

    await page.addInitScript((stored) => localStorage.setItem('lotr-session', stored), JSON.stringify(session))
    await recordTransitions(page)
    await page.goto(`/#/games/${id}`)
    await expect(page.locator('.hand [data-card-id]').first()).toBeVisible()

    await use({
      id,
      playerId,
      state,
      debug: async (message) => void (await ok(request.post(`/api/games/${id}/debug`, { headers, data: message }), false)),
      answer: async (choice) => {
        const { game } = await state()
        await ok(request.post(`/api/games/${id}`, { headers, data: { choice, questionVersion: game.questionVersion, player: playerId } }), false)
      },
    })

    await request.delete(`/api/games/${id}`, { headers })
  },
})

const ok = async (response: ReturnType<APIRequestContext['get']>, json = true) => {
  const r = await response
  expect(r.ok(), await r.text()).toBe(true)
  return json ? r.json() : null
}

/** Keep, for each view transition, the animations it runs on cards, e.g. "::view-transition-new(card-…) deal-turn-up". */
const recordTransitions = (page: Page) =>
  page.addInitScript(() => {
    const w = window as typeof window & { transitions: string[][] }
    w.transitions = []
    const start = document.startViewTransition.bind(document)
    document.startViewTransition = ((update: () => Promise<void>) => {
      const transition = start(update)
      transition.ready.then(() =>
        w.transitions.push(
          document
            .getAnimations()
            .map((a) => `${(a.effect as KeyframeEffect).pseudoElement} ${(a as CSSAnimation).animationName}`)
            .filter((s) => s.includes('(card-')),
        ),
      )
      return transition
    }) as typeof document.startViewTransition
  })

const transitions = (page: Page) => page.evaluate(() => (window as typeof window & { transitions: string[][] }).transitions)
const handIds = (page: Page) => page.locator('.hand [data-card-id]').evaluateAll((cards) => cards.map((c) => (c as HTMLElement).dataset.cardId!))

test('drawn cards fly from the deck, turning over on the way', async ({ page, game }) => {
  const before = await handIds(page)
  await game.debug({ tag: 'DrawCards', contents: { player: game.playerId, count: 2 } })

  await expect.poll(async () => (await handIds(page)).length).toBe(before.length + 2)
  const drawn = (await handIds(page)).filter((id) => !before.includes(id))
  await expect
    .poll(async () => (await transitions(page)).flat())
    .toEqual(expect.arrayContaining(drawn.map((id) => `::view-transition-new(card-${id}) deal-turn-up`)))
})

test('a played card waits in hand while it is paid for, then moves into play', async ({ page, game }) => {
  const cards = await (await page.request.get('/api/cards')).json()
  const { game: g } = await game.state()
  for (const hero of Object.keys(g.entities.heroes))
    await game.debug({ tag: 'PlaceTokens', contents: [{ tag: 'GameSource' }, { tag: 'HeroTarget', contents: hero }, 'Resource', 5] })

  // an ally the starter heroes (all Leadership) can pay for between them
  const affordable = (c: { cardCode: string }) => {
    const def = cards[c.cardCode]
    return def.cardType === 'AllyType' && ['Leadership', 'Neutral'].includes(def.sphere) && def.cost?.contents > 1
  }
  let ally: { id: string } | undefined
  for (let draws = 0; !(ally = (await game.state()).game.players[game.playerId].hand.find(affordable)); draws++) {
    expect(draws, 'no affordable ally in the deck').toBeLessThan(30)
    await game.debug({ tag: 'DrawCards', contents: { player: game.playerId, count: 1 } })
  }

  await game.debug({ tag: 'PlayCard', contents: [game.playerId, ally.id] })
  await expect(page.locator(`.hand [data-card-id="${ally.id}"].playing`)).toBeVisible()

  for (let pays = 0; (await game.state()).game.question[game.playerId]?.label?.startsWith('Pay'); pays++) {
    expect(pays, 'still paying').toBeLessThan(10)
    await game.answer(0)
  }

  await expect(page.locator(`.play-area [data-card-id="${ally.id}"]`)).toBeVisible()
  await expect(page.locator(`.hand [data-card-id="${ally.id}"]`)).toHaveCount(0)
  // it morphed out of the hand: the move had an old snapshot of the card to start from
  await expect
    .poll(async () => (await transitions(page)).at(-1))
    .toEqual(expect.arrayContaining([expect.stringMatching(new RegExp(`^::view-transition-old\\(card-${ally.id}\\)`))]))
})
