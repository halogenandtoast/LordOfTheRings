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

export const createGame = (game: { name: string; seed?: number }) =>
  apiFetch('/api/games', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(game),
  }).then((r) => json<{ id: number }>(r))

export const deleteGame = (id: number) => apiFetch(`/api/games/${id}`, { method: 'DELETE' })

/** Download a game as a JSON file. */
export const exportGame = async (id: string | number) => {
  const response = await apiFetch(`/api/games/${id}/export`)
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  const filename = /filename="([^"]+)"/.exec(response.headers.get('Content-Disposition') ?? '')?.[1] ?? `game-${id}.json`
  const url = URL.createObjectURL(await response.blob())
  const link = Object.assign(document.createElement('a'), { href: url, download: filename })
  link.click()
  URL.revokeObjectURL(url)
}

/** Load an exported game file as a new game. */
export const importGame = async (file: File) =>
  apiFetch('/api/import', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: await file.text(),
  }).then((r) => json<{ id: number }>(r))
