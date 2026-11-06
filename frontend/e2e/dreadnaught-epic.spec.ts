import { expect, request, test } from '@playwright/test'

// This creates an event that cannot be deleted group by group. Point EPIC_API_URL
// at an API backed by a disposable database before running this regression.
test('Dreadnaught Epic contributions survive concurrent writes and undo', async () => {
  test.skip(!process.env.EPIC_API_URL, 'Requires EPIC_API_URL and a disposable database')
  const api = await request.newContext({ baseURL: process.env.EPIC_API_URL })
  const call = async (method: string, path: string, token?: string, data?: unknown, status = 200) => {
    const response = await api.fetch(`/api${path}`, { method, headers: token ? { Authorization: `Token ${token}` } : {}, data })
    expect(response.status(), await response.text()).toBe(status)
    const text = await response.text()
    return text && status === 200 ? JSON.parse(text) : undefined
  }
  try {
    const register = async (suffix: string) => {
      const username = `dread-epic-${Date.now()}-${suffix}`
      return call('POST', '/register', undefined, { username, email: `${username}@example.com`, password: 'epic-regression-password' })
    }
    const ta = (await register('organizer')).token
    const tb = (await register('second')).token
    const eid = (await call('POST', '/epic-events', ta, { name: 'Independent fleets', difficulty: 'NormalMode' })).id
    const group = async (token: string, name: string) => (await call('POST', `/epic-events/${eid}/groups`, token, { name, seats: 1 })).gameId
    const ga = await group(ta, 'First fleet')
    const gb = await group(tb, 'Second fleet')
    const game = async (id: string, token: string) => (await call('GET', `/games/${id}`, token)).game
    const event = () => call('GET', `/epic-events/${eid}`, ta)
    const debug = (id: string, token: string, tag: string, contents: unknown) => call('POST', `/games/${id}/debug`, token, { tag, contents })
    const answer = (id: string, token: string, g: any, choice: number, status = 200) => call('POST', `/games/${id}`, token, { player: Object.keys(g.question)[0], questionVersion: g.questionVersion, choice }, status)
    const undo = (id: string, token: string, status = 200) => call('POST', `/games/${id}/undo`, token, {}, status)
    const progress = async (id: string, token: string, amount: number) => {
      const qid = Object.keys((await game(id, token)).entities.quests)[0]
      await debug(id, token, 'PlaceTokens', [{ tag: 'GameSource' }, { tag: 'QuestTarget', contents: qid }, 'Progress', amount])
    }
    const setup = async (id: string, token: string) => {
      for (let n = 0; n < 12; n++) {
        const g = await game(id, token)
        if (g.round > 0) return
        let q: any = Object.values(g.question)[0]
        while (q.question) q = q.question
        const ship = q.choices.findIndex((c: any) => c.messages?.some((m: any) => m.tag === 'PutObjectiveAllyIntoPlayUnder' && m.contents[1].cardCode === 'the-hunt-for-the-dreadnaught-009'))
        const keep = q.choices.findIndex((c: any) => c.label === '$label.keepHand')
        await answer(id, token, g, ship >= 0 ? ship : keep >= 0 ? keep : 0)
      }
      throw new Error('Fleet setup did not finish')
    }
    await call('POST', `/epic-events/${eid}/start`, ta, {}, 409)
    await call('POST', `/games/${ga}/join`, ta, {})
    await call('POST', `/games/${gb}/join`, tb, {})
    expect((await call('GET', `/games/${ga}`, ta)).started).toBe(false)
    await call('POST', `/epic-events/${eid}/start`, tb, {}, 404)
    expect((await call('POST', `/epic-events/${eid}/start`, ta, {})).totalPlayers).toBe(2)
    await call('POST', `/epic-events/${eid}/groups`, ta, { name: 'Too late', seats: 1 }, 409)
    await call('POST', `/games/${ga}/reset`, ta, {}, 409)
    await call('POST', `/games/${gb}/leave`, tb, {}, 409)
    await call('GET', `/games/${gb}`, ta, undefined, 404)
    expect((await event()).groups.find((g: any) => g.gameId === gb).canOpen).toBe(false)
    await setup(ga, ta)
    await setup(gb, tb)
    await progress(ga, ta, 4)
    await progress(gb, tb, 3)
    expect((await event()).progress).toBe(7)
    expect((Object.values((await game(ga, ta)).entities.quests)[0] as any).tokens.Progress).toBe(4)
    await undo(ga, ta)
    expect((await event()).progress).toBe(3)
    await Promise.all([progress(ga, ta, 2), progress(gb, tb, 2)])
    expect((await event()).progress).toBe(7)
    await Promise.all([progress(ga, ta, 1), progress(ga, ta, 1)])
    expect((await event()).progress).toBe(9)
    const stale = await game(ga, ta)
    await debug(gb, tb, 'AddToStaging', { cardCode: 'the-hunt-for-the-dreadnaught-023', id: crypto.randomUUID() })
    await answer(ga, ta, stale, 0, 409)
    const enemy = Object.entries((await game(gb, tb)).entities.enemies).find(([, e]) => (e as any).cardCode === 'the-hunt-for-the-dreadnaught-023')![0]
    await debug(ga, ta, 'QueueEpicCommand', [1, 'the-hunt-for-the-dreadnaught-007-b', { tag: 'EpicDamage', contents: 1 }, { tag: 'EnemyTarget', contents: enemy }])
    expect((await game(gb, tb)).entities.enemies[enemy].tokens.Damage).toBe(1)
    await undo(ga, ta, 409)
    await undo(gb, tb, 409)
    const advance = async (id: string, token: string) => {
      await debug(id, token, 'CompleteQuest', Object.keys((await game(id, token)).entities.quests)[0])
      for (let n = 0; n < 8; n++) {
        const g = await game(id, token)
        if (g.outcome || Object.values(g.entities.enemies).some((e: any) => e.cardCode === 'the-hunt-for-the-dreadnaught-013')) return
        await answer(id, token, g, 0)
      }
      throw new Error('Dreadnaught did not enter play')
    }
    await advance(ga, ta)
    await debug(ga, ta, 'ContributeShared', ['DreadnaughtDamage', 30])
    expect((await event()).damage).toBe(30)
    expect((await game(ga, ta)).outcome).toBe('Won')
    await advance(gb, tb)
    expect((await game(gb, tb)).outcome).toBe('Won')
  } finally { await api.dispose() }
})
