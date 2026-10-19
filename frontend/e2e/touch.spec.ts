import { expect, test } from '@playwright/test'

// On a phone the board fits the screen, and cards are read by touch rather than hover.
test.use({ hasTouch: true, isMobile: true, viewport: { width: 390, height: 844 } })

test('a tap reads a card, and the next tap only puts it away', async ({ page, request }) => {
  const name = `e2e-${Date.now()}-${Math.floor(Math.random() * 1e6)}`
  const session = await (await request.post('/api/register', { data: { username: name, email: `${name}@example.com`, password: 'e2e-password' } })).json()
  const headers = { Authorization: `Token ${session.token}` }
  const { id } = await (await request.post('/api/games', { headers, data: { name, seed: 7 } })).json()
  await page.addInitScript((stored) => localStorage.setItem('lotr-session', stored), JSON.stringify(session))
  await page.goto(`/#/games/${id}`)

  // the question is docked under the board, and the log is in a drawer that starts shut
  const keep = page.locator('.docked-question button').first()
  await expect(keep).toBeVisible()
  await expect(page.locator('.sidebar')).toHaveCount(0)

  await page.locator('.hero-column [data-card-id]').first().tap()
  await expect(page.locator('.card-overlay')).toBeVisible()

  // dismissing over an answer must not give it
  await keep.tap()
  await expect(page.locator('.card-overlay')).toHaveCount(0)
  await expect(page.locator('.docked-question .prompt')).toHaveText('Keep this starting hand?')

  await request.delete(`/api/games/${id}`, { headers })
})
