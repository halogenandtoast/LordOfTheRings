import { expect, test, type APIRequestContext } from '@playwright/test'

const register = async (request: APIRequestContext) => {
  const username = `campaign-${Date.now()}-${Math.floor(Math.random() * 1e8)}`
  const response = await request.post('/api/register', { data: { username, email: `${username}@example.com`, password: 'e2e-password' } })
  expect(response.ok(), await response.text()).toBe(true)
  const session = await response.json()
  return { Authorization: `Token ${session.token}` }
}

const json = async (response: Awaited<ReturnType<APIRequestContext['get']>>) => {
  expect(response.ok(), await response.text()).toBe(true)
  return response.json()
}

// Exercise the real resolution/continuation path, using debug only to win each
// scenario immediately. Resolve any reward choices before advancing to the next game.
const finish = async (request: APIRequestContext, headers: Record<string, string>, campaign: string, games: string[], campaigns: string[]) => {
  const created = await json(await request.post('/api/campaigns', { headers, data: { name: 'Campaign API regression', campaign, seats: 1, publicScoring: false } }))
  campaigns.push(created.campaignId)
  let id = created.id
  for (;;) {
    games.push(id)
    expect((await request.post(`/api/games/${id}/join`, { headers, data: {} })).ok()).toBe(true)
    expect((await request.post(`/api/games/${id}/debug`, { headers, data: { tag: 'GameOver', contents: 'Won' } })).ok()).toBe(true)
    for (let n = 0; n < 20; n++) {
      const state = await json(await request.get(`/api/games/${id}`, { headers }))
      if (state.game.outcome) break
      const player = Object.keys(state.game.question)[0]
      expect(player, 'a pending resolution should offer a choice').toBeTruthy()
      expect((await request.post(`/api/games/${id}`, { headers, data: { player, choice: 0, questionVersion: state.game.questionVersion } })).ok()).toBe(true)
    }
    const view = await json(await request.get(`/api/campaigns/${created.campaignId}`, { headers }))
    if (view.status === 'finished') return { id: created.campaignId, view }
    expect(view.status).toBe('won')
    const previousId = id
    const before = await json(await request.get(`/api/games/${previousId}`, { headers }))
    id = (await json(await request.post(`/api/campaigns/${created.campaignId}/next`, { headers }))).id
    const locked = await json(await request.get(`/api/games/${previousId}`, { headers }))
    expect(locked.canUndo).toBe(false)
    for (const target of ['', '?to=phase', '?to=round', '?to=step&step=0']) {
      expect((await request.post(`/api/games/${previousId}/undo${target}`, { headers })).status()).toBe(409)
    }
    expect((await json(await request.get(`/api/games/${previousId}`, { headers }))).game).toEqual(before.game)
  }
}

const cleanup = async (request: APIRequestContext, headers: Record<string, string>, games: string[], campaigns: string[]) => {
  for (const id of games) await request.delete(`/api/games/${id}`, { headers })
  for (const id of campaigns) await request.delete(`/api/campaigns/${id}`, { headers })
}

test('only the owner can extend a completed Core campaign, preserving its log', async ({ request }) => {
  const headers = await register(request)
  const outsider = await register(request)
  const games: string[] = [], campaigns: string[] = []
  try {
    const source = await finish(request, headers, 'revised-core', games, campaigns)
    expect((await request.post(`/api/campaigns/${source.id}/extend`)).status()).toBe(401)
    expect((await request.post(`/api/campaigns/${source.id}/extend`, { headers: outsider })).status()).toBe(404)
    const next = await json(await request.post(`/api/campaigns/${source.id}/extend`, { headers }))
    games.push(next.id)
    const lastGame = games[games.length - 2]
    expect((await json(await request.get(`/api/games/${lastGame}`, { headers }))).canUndo).toBe(false)
    expect((await request.post(`/api/games/${lastGame}/undo`, { headers })).status()).toBe(409)
    const view = await json(await request.get(`/api/campaigns/${source.id}`, { headers }))
    expect(view.log).toEqual({ ...source.view.log, campaign: 'mirkwood-paths' })
    expect(view.next).toBe('the-oath')
    expect(view.status).toBe('playing')
    expect(view.scenarios).toHaveLength(5)
    expect((await request.post(`/api/campaigns/${source.id}/extend`, { headers })).status()).toBe(409)
  } finally { await cleanup(request, headers, games, campaigns) }
})

test('Ranger Sense reward is optional per seat and redeemed once for the next campaign', async ({ request }) => {
  const headers = await register(request)
  const outsider = await register(request)
  const games: string[] = [], campaigns: string[] = []
  try {
    const source = await finish(request, headers, 'dark-of-mirkwood', games, campaigns)
    expect(source.view.canClaimRangerSense).toBe(true)
    const data = { name: 'Ranger reward', campaign: 'revised-core', seats: 2, rewardCampaign: source.id, rewardSeats: [1], publicScoring: false }
    expect((await request.post('/api/campaigns', { headers: outsider, data })).status()).toBe(404)
    expect((await request.post('/api/campaigns', { headers, data: { ...data, rewardSeats: [2] } })).status()).toBe(400)
    const declined = await json(await request.post('/api/campaigns', { headers, data: { ...data, rewardSeats: [] } }))
    games.push(declined.id); campaigns.push(declined.campaignId)
    expect((await json(await request.get(`/api/campaigns/${source.id}`, { headers }))).canClaimRangerSense).toBe(true)
    const next = await json(await request.post('/api/campaigns', { headers, data }))
    games.push(next.id); campaigns.push(next.campaignId)
    const view = await json(await request.get(`/api/campaigns/${next.campaignId}`, { headers }))
    expect(view.log.pool).toEqual([{ code: 'dark-of-mirkwood-049', seat: 1, attachedTo: null }])
    expect(view.log.completed).toEqual([])
    expect((await json(await request.get(`/api/campaigns/${source.id}`, { headers }))).canClaimRangerSense).toBe(false)
    expect((await request.post('/api/campaigns', { headers, data })).status()).toBe(409)
  } finally { await cleanup(request, headers, games, campaigns) }
})

test('campaign games cannot be deleted individually and deleting the campaign removes all its games', async ({ request }) => {
  const headers = await register(request)
  const outsider = await register(request)
  const created = await json(await request.post('/api/campaigns', { headers, data: { name: 'Campaign deletion regression', campaign: 'revised-core', seats: 1, publicScoring: false } }))
  try {
    expect((await request.delete(`/api/games/${created.id}`, { headers })).status()).toBe(409)
    expect((await request.delete(`/api/campaigns/${created.campaignId}`, { headers: outsider })).status()).toBe(404)
    expect((await request.get(`/api/games/${created.id}`, { headers })).ok()).toBe(true)
    expect((await request.delete(`/api/campaigns/${created.campaignId}`, { headers })).ok()).toBe(true)
    expect((await request.get(`/api/games/${created.id}`, { headers })).status()).toBe(404)
    expect((await request.get(`/api/campaigns/${created.campaignId}`, { headers })).status()).toBe(404)
  } finally {
    await request.delete(`/api/campaigns/${created.campaignId}`, { headers })
  }
})
