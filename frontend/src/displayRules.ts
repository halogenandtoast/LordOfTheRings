// What to show of content still being worked on, as in Arkham: alpha to those who have
// opened ?alpha once, beta to beta users, dev in a dev build with alpha on. Unimplemented
// content shows only in a dev build, and cannot be chosen.
export type Gateable = { alpha?: boolean; beta?: boolean; dev?: boolean; unimplemented?: boolean }

export type DisplayRuleOptions = {
  alpha: boolean
  beta: boolean
  dev: boolean
}

export const isDevBuild = () => !import.meta.env.PROD

export const shouldDisplay = (item: Gateable, { alpha, beta, dev }: DisplayRuleOptions) => {
  if (item.unimplemented) return dev
  if (item.dev) return dev && alpha
  if (item.beta) return beta
  if (item.alpha) return alpha
  return true
}

export const filterDisplayable = <T extends Gateable>(items: T[], options: DisplayRuleOptions) =>
  items.filter((item) => shouldDisplay(item, options))

// ?alpha turns alpha on for good in this browser
export const alphaEnabled = (query: Record<string, unknown>) => {
  try {
    if (query.alpha !== undefined) localStorage.setItem('alpha', 'true')
    return query.alpha !== undefined || localStorage.getItem('alpha') === 'true'
  } catch {
    return query.alpha !== undefined
  }
}
