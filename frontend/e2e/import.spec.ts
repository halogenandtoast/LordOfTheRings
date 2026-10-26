import { expect, test } from '@playwright/test'

// Preview selection is local; it must never create a game or navigate to an unsaved one.
test('an exported game previews its players and progress instead of its filename', async ({ page }) => {
  await page.addInitScript(() => localStorage.setItem('lotr-session', JSON.stringify({
    token: 'preview-test', user: { id: 1, username: 'Preview', email: '', admin: false, beta: false },
  })))
  await page.route('**/api/**', route => {
    if (route.request().method() !== 'GET') throw new Error('Selecting a preview must not write to the server')
    return route.fulfill({ json: route.request().url().endsWith('/cards') ? {} : [] })
  })
  await page.goto('/#/')
  await page.getByRole('button', { name: 'Load game', exact: true }).click()
  const panel = page.locator('.load-game-panel')
  const picker = panel.locator('input[type=file]')
  const exported = {
    version: 1, name: 'Passage Through Mirkwood', playerName: 'The Fellowship',
    game: {
      scenario: { name: 'Passage Through Mirkwood', victoryDisplay: [], easyMode: true },
      round: 4, phase: 'TravelPhase', outcome: null, playerOrder: ['one', 'two'],
      players: {
        one: { name: 'First Player', threat: 28, eliminated: false },
        two: { name: 'Second Player', threat: 31, eliminated: false },
      },
      entities: { heroes: {
        aragorn: { owner: 'one', cardCode: '01001' },
        eowyn: { owner: 'two', cardCode: '01007' },
      } },
    },
  }
  await picker.setInputFiles({ name: 'opaque-hash.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(exported)) })
  const preview = panel.locator('.import-preview')
  await expect(preview).toContainText('Passage Through Mirkwood')
  await expect(preview).toContainText('round 4')
  await expect(preview).toContainText('travel')
  await expect(preview.locator('.seat-name')).toHaveText(['First Player', 'Second Player'])
  await expect(preview.locator('.hero')).toHaveCount(2)
  await expect(preview.locator('a')).toHaveCount(0)
  await expect(panel).not.toContainText('opaque-hash.json')
  await expect(panel.getByRole('button', { name: 'Load game', exact: true })).toBeEnabled()

  await picker.setInputFiles({ name: 'invalid.json', mimeType: 'application/json', buffer: Buffer.from('{}') })
  await expect(preview).toHaveCount(0)
  await expect(panel.locator('.import-error')).toBeVisible()
  await expect(panel.getByRole('button', { name: 'Load game', exact: true })).toHaveCount(0)
})
