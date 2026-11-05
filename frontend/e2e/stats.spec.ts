import { expect, test } from '@playwright/test'

const line = { started: 5, won: 2, lost: 1, score: { lowest: 100, highest: 120, average: 110 }, rounds: { lowest: 5, highest: 9, average: 7 } }

test('statistics explain results and expand hero lineups with the keyboard', async ({ page }) => {
  await page.route('**/api/stats', route => route.fulfill({ json: [{ scenario: 'test', name: 'Test scenario', mode: 'NormalMode', total: line, decks: [{ heroes: ['01001'], stats: line }] }] }))
  await page.goto('/#/stats')
  await expect(page.getByText('66.7%')).toHaveCount(2)
  await expect(page.getByText('Scores include victories only;', { exact: false })).toBeVisible()
  const expand = page.getByRole('button', { name: 'Test scenario' })
  await expand.focus()
  await page.keyboard.press('Enter')
  await expect(expand).toHaveAttribute('aria-expanded', 'true')
  await expect(page.locator('tr.deck')).toHaveCount(1)
  await page.setViewportSize({ width: 390, height: 844 })
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true)
})

test('statistics handle no victories and empty results', async ({ page }) => {
  await page.route('**/api/stats', route => route.fulfill({ json: [{ scenario: 'test', name: 'No victories', mode: 'EasyMode', total: { ...line, won: 0, score: null }, decks: [] }] }))
  await page.goto('/#/stats')
  await expect(page.locator('tr.scenario')).toContainText('0.0%')
  await expect(page.locator('tr.scenario td').nth(5)).toContainText('—')
  await page.unroute('**/api/stats')
  await page.route('**/api/stats', route => route.fulfill({ json: [] }))
  await page.reload()
  await expect(page.getByText('No games have been played yet.')).toBeVisible()
})
