// What to show of content still being worked on, as in Arkham: alpha is playable by those
// who have opened ?alpha once, and shows to everyone else as unplayable. Beta is playable
// by everyone, with a warning that it may have bugs. Dev content shows only in a dev build with alpha on. Unimplemented content
// shows only in a dev build, and cannot be chosen.
export type Gateable = { alpha?: boolean; beta?: boolean; dev?: boolean; unimplemented?: boolean }

export type DisplayRuleOptions = {
  alpha: boolean
  dev: boolean
}

export const isDevBuild = () => !import.meta.env.PROD

export const shouldDisplay = (item: Gateable, { alpha, dev }: DisplayRuleOptions) => {
  if (item.unimplemented) return dev
  if (item.dev) return dev && alpha
  return true
}

export const canPlay = (item: Gateable, options: DisplayRuleOptions) => {
  if (item.unimplemented) return false
  if (item.alpha) return options.alpha
  return shouldDisplay(item, options)
}

export const filterDisplayable = <T extends Gateable>(items: T[], options: DisplayRuleOptions) =>
  items.filter((item) => shouldDisplay(item, options)).map((item) => ({ ...item, playable: canPlay(item, options) }))

// ?alpha turns alpha on for good in this browser
export const alphaEnabled = (query: Record<string, unknown>) => {
  try {
    if (query.alpha !== undefined) localStorage.setItem('alpha', 'true')
    return query.alpha !== undefined || localStorage.getItem('alpha') === 'true'
  } catch {
    return query.alpha !== undefined
  }
}
