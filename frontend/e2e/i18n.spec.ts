import { expect, test } from '@playwright/test'

// Every page renders its messages: vue-i18n warns about a missing key and shows the key itself.
test('no page is missing a message', async ({ page, request }) => {
  const name = `e2e-${Date.now()}-${Math.floor(Math.random() * 1e6)}`
  const session = await (await request.post('/api/register', { data: { username: name, email: `${name}@example.com`, password: 'e2e-password' } })).json()
  const headers = { Authorization: `Token ${session.token}` }
  const { id } = await (await request.post('/api/games', { headers, data: { name, seed: 7 } })).json()
  const warnings: string[] = []
  page.on('console', (message) => {
    if (message.text().includes('[intlify]')) warnings.push(message.text())
  })
  await page.addInitScript((stored) => localStorage.setItem('lotr-session', stored), JSON.stringify(session))

  for (const path of ['/#/', '/#/new-game', `/#/games/${id}`, '/#/cards', '/#/cards/01001', '/#/decks']) {
    await page.goto(path)
    await page.waitForLoadState('networkidle')
    // a key shown in place of its message, like "game.bar.undo"
    await expect(page.locator('body')).not.toContainText(/\b(nav|auth|home|newGame|common|game|browser)\.[a-zA-Z]/)
  }

  await request.delete(`/api/games/${id}`, { headers })
  expect(warnings).toEqual([])
})
