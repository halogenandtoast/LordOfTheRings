/**
 * The deck endpoints, through the app's signed fetch. The server answers a refused
 * request with a sentence written for a person, which becomes the error's message.
 */
import { apiFetch } from '../../api'
import type { Deck, DeckSummary, ImportRequest } from '@browser/types/deck'

async function request<T>(path: string, init: RequestInit = {}): Promise<T> {
  const response = await apiFetch(path, init)
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  const text = await response.text()
  return (text === '' ? undefined : JSON.parse(text)) as T
}

export const fetchDecks = () => request<DeckSummary[]>('/api/decks')

export const fetchDeck = (id: number) => request<Deck>(`/api/decks/${id}`)

/** Import a deck from a RingsDB link or the contents of a JSON file. */
export const importDeck = (body: ImportRequest) =>
  request<Deck>('/api/decks', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  })

export const deleteDeck = (id: number) => request<void>(`/api/decks/${id}`, { method: 'DELETE' })
