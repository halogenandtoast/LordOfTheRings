import { expect, test } from '@playwright/test'

// The card browser reads the static card database, so it needs no account or game.

test('a quest reverse scanned on its own reads and opens like its sibling', async ({ page }) => {
  await page.goto('/#/cards')
  await page.locator('.toolbar__search').fill("Beorn's Path")
  await expect(page.locator('.list__button')).toHaveCount(1)
  await page.locator('.list__button').click()
  const dialog = page.locator('dialog[open]')
  // A Chosen Path's 3A is the front, and Beorn's Path is the other side, as with "Don't Leave the Path!"
  await expect(dialog.locator('img').first()).toHaveAttribute('src', /core-set-121-s3\.avif$/)
  await expect(dialog.locator('h2, h1').first()).toContainText('A Chosen Path')
  await expect(dialog.locator('.block--back')).toContainText("Beorn's Path")
  await expect(dialog.locator('.block--back')).toContainText("Ungoliant's Spawn is in play")
})
