import type { GameSummary } from './server'

const json = async <T>(response: Response): Promise<T> => {
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  return (await response.json()) as T
}

export const fetchGames = () => fetch('/api/games').then((r) => json<GameSummary[]>(r))

export const createGame = (game: { name: string; playerName: string; seed?: number }) =>
  fetch('/api/games', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(game),
  }).then((r) => json<{ id: string }>(r))

export const deleteGame = (id: string) => fetch(`/api/games/${id}`, { method: 'DELETE' })
