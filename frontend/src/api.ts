import type { DeckList } from './browser/types/deck'
import { authHeaders, signOut } from './auth'
import { router } from './router'
import type { GameSummary } from './server'

/** A fetch that signs the request and sends a signed-out user to sign in. */
export const apiFetch = async (path: string, init: RequestInit = {}): Promise<Response> => {
  const response = await fetch(path, { ...init, headers: { ...authHeaders(), ...(init.headers ?? {}) } })
  if (response.status === 401) {
    signOut()
    await router.push({ path: '/sign-in', query: { next: router.currentRoute.value.fullPath } })
  }
  return response
}

const json = async <T>(response: Response): Promise<T> => {
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  return (await response.json()) as T
}

export const fetchGames = () => apiFetch('/api/games').then((r) => json<GameSummary[]>(r))

/** Games in progress that are open to spectators. */
export const fetchSpectatableGames = () => apiFetch('/api/spectatable').then((r) => json<GameSummary[]>(r))

export interface AdminDashboard {
  users: number
  activeUsers: number
  games: number
  recentGames: GameSummary[]
}

export const fetchAdminDashboard = () => apiFetch('/api/admin').then((r) => json<AdminDashboard>(r))

/** A core starter deck by sphere, or one of the user's imported decks, and whether its
 * cards follow the FAQ's errata (the server takes them to unless told otherwise). */
export type DeckChoice = ({ starter: string } | { deckId: number } | { list: DeckList }) & { errata?: boolean }

export const createGame = (game: { name: string; seed?: number; scenario?: string; deck?: DeckChoice; decks?: DeckChoice[]; seats?: number; allowSpectators?: boolean; easyMode?: boolean; nightmare?: boolean }) =>
  apiFetch('/api/games', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(game),
  }).then((r) => json<{ id: string }>(r))

export const deleteGame = (id: string) => apiFetch(`/api/games/${id}`, { method: 'DELETE' })

/** Download a game as a JSON file. */
export const exportGame = async (id: string) => {
  const response = await apiFetch(`/api/games/${id}/export`)
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  const filename = /filename="([^"]+)"/.exec(response.headers.get('Content-Disposition') ?? '')?.[1] ?? `game-${id}.json`
  const url = URL.createObjectURL(await response.blob())
  const link = Object.assign(document.createElement('a'), { href: url, download: filename })
  link.click()
  URL.revokeObjectURL(url)
}

/** Upload a game's export for a bug report, giving back where it went. */
export const fileBug = (id: string) =>
  apiFetch(`/api/games/${id}/file-bug`, { method: 'POST' }).then((r) => json<{ url: string }>(r))

/** Load an exported game file as a new game. */
export const importGame = async (file: File) =>
  apiFetch('/api/import', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: await file.text(),
  }).then((r) => json<{ id: string }>(r))

/** A boon or burden earned: a player card for a seat's deck, or one Permanent on a hero. */
export interface PoolCard {
  code: string
  seat: number | null
  attachedTo: string | null
}

/** The campaign log, as the engine keeps it between a campaign's scenarios. */
export interface CampaignLog {
  campaign: string
  heroes: string[][]
  fallenHeroes: string[]
  threatPenalty: number
  pool: PoolCard[]
  notes: Record<string, string[]>
  /** Numbers noted for cards, by note, as the damage on Protect the Innocent. */
  counts?: Record<string, Record<string, number>>
  completed: { scenario: string; score: number }[]
}

export interface CampaignView {
  id: string
  name: string
  campaignName: string
  log: CampaignLog
  scenarios: { id: string; name: string; card: string }[]
  /** Newest first. */
  games: { id: string; scenario: string; outcome: 'Won' | 'Lost' | null }[]
  next: string | null
  status: 'playing' | 'won' | 'lost' | 'finished'
}

/** Start a campaign: its first scenario's game, waiting for its seats. */
export const createCampaign = (campaign: { name: string; campaign: string; seats?: number; allowSpectators?: boolean; easyMode?: boolean; nightmare?: boolean }) =>
  apiFetch('/api/campaigns', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(campaign),
  }).then((r) => json<{ id: string; campaignId: string }>(r))

export const fetchCampaign = (id: string) => apiFetch(`/api/campaigns/${id}`).then((r) => json<CampaignView>(r))

/** The next scenario's game, or the last one's again when it was lost. */
export const continueCampaign = (id: string) =>
  apiFetch(`/api/campaigns/${id}/next`, { method: 'POST' }).then((r) => json<{ id: string }>(r))
