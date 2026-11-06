import { expect, test } from '@playwright/test'

for (const status of ['playing', 'won', 'lost', 'finished']) {
  test(`campaign is grouped under ${status === 'finished' ? 'Finished' : 'Active'} when ${status}`, async ({ page }) => {
    await page.addInitScript(() => localStorage.setItem('lotr-session', JSON.stringify({ token: 'fixture', user: { id: 1, username: 'Campaign player' } })))
    await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Campaign player' } }))
    await page.route('**/api/epic-events', route => route.fulfill({ json: [] }))
    const base = { playerName: 'Campaign player', scenario: 'Passage Through Mirkwood', round: 5, phase: 'QuestPhase', threat: 30, victoryPoints: 3, waiting: false, easyMode: false, nightmare: false, seats: [{ name: 'Campaign player', threat: 30, eliminated: false, heroes: [{ code: '01001', sphere: 'Leadership' }, { code: '01002', sphere: 'Leadership' }] }] }
    await page.route('**/api/games', route => route.fulfill({ json: [
      { ...base, id: 'latest', name: 'Latest scenario', campaignId: 'campaign', outcome: status === 'playing' ? null : 'Won' },
      { ...base, id: 'previous', name: 'Previous attempt', campaignId: 'campaign', outcome: 'Lost' },
      { ...base, id: 'standalone', name: 'Standalone game', outcome: 'Won' },
    ] }))
    let deleted = false
    await page.route('**/api/campaigns/campaign', route => {
      if (route.request().method() === 'DELETE') {
        deleted = true
        return route.fulfill({ status: 204 })
      }
      return route.fulfill({ json: {
      id: 'campaign', name: 'Mirkwood campaign', campaignName: 'Revised Core', status, next: status === 'finished' ? null : 'anduin',
      scenarios: [{ id: 'passage', name: 'Passage Through Mirkwood' }, { id: 'anduin', name: 'Journey Along the Anduin' }],
      games: [{ id: 'latest', scenario: 'passage' }, { id: 'previous', scenario: 'passage' }],
      log: { completed: [{ scenario: 'passage', score: 120 }] },
    } })
    })
    await page.goto('/#/')
    // Explicitly select the expected tab, including the finished-only default.
    await page.locator(`input[value="${status === 'finished' ? 'finished' : 'active'}"]`).locator('..').click()
    const campaign = page.locator('.campaign-row')
    await expect(campaign).toHaveCount(1)
    await expect(campaign.locator('.title')).toHaveText('Revised Core')
    await expect(campaign.locator('.campaign-header')).toHaveAttribute('href', '#/campaigns/campaign')
    await expect(campaign.locator('.progress')).toContainText('1 / 2')
    await expect(campaign.getByText('Campaign log', { exact: true })).toHaveCount(0)
    if (status === 'playing') {
      await expect(campaign.locator('a.campaign-details')).toHaveAttribute('href', '#/games/latest')
      await expect(campaign.locator('.scenario-name')).toHaveText('Passage Through Mirkwood')
      await expect(campaign.locator('.scenario-progress')).toContainText('5')
    } else {
      await expect(campaign.locator('a.campaign-details')).toHaveCount(0)
      await expect(campaign.locator('.scenario-heading')).toHaveCount(0)
    }
    await expect(campaign.locator('.heroes .hero')).toHaveCount(2)
    await expect(campaign.locator('.seat-name')).toHaveText('Campaign player')
    await expect(campaign.locator('.seat-threat')).toContainText('30')
    await expect(page.locator('.game a.title').filter({ hasText: 'Standalone game' })).toHaveCount(status === 'finished' ? 1 : 0)
    await expect(campaign.locator('details, .game')).toHaveCount(0)
    await expect(campaign.getByRole('button', { name: 'Delete campaign', exact: true })).toHaveCount(1)
    await page.locator(`input[value="${status === 'finished' ? 'active' : 'finished'}"]`).locator('..').click()
    await expect(page.locator('.campaign-row')).toHaveCount(0)
    await expect(page.locator('.game a.title').filter({ hasText: 'Standalone game' })).toHaveCount(status === 'finished' ? 0 : 1)
    await page.locator(`input[value="${status === 'finished' ? 'finished' : 'active'}"]`).locator('..').click()
    await campaign.getByRole('button', { name: 'Delete campaign', exact: true }).click()
    const dialog = campaign.locator('dialog[open]')
    await expect(dialog).toContainText('all its games')
    await dialog.getByRole('button', { name: 'Cancel', exact: true }).click()
    expect(deleted).toBe(false)
    await expect(campaign).toHaveCount(1)
    await campaign.getByRole('button', { name: 'Delete campaign', exact: true }).click()
    await dialog.getByRole('button', { name: 'Delete', exact: true }).click()
    await expect(campaign).toHaveCount(0)
    expect(deleted).toBe(true)
  })
}
