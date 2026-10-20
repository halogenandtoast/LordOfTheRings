import { computed, nextTick, onScopeDispose, readonly, ref, shallowRef } from 'vue'
import { useI18n } from 'vue-i18n'
import { apiFetch, type DeckChoice } from '../api'
import { token } from '../auth'
import type { CardDef, GameData, ServerMessage } from '../server'
import type { GameView } from '../types'
import { toView } from '../view'
import { beforeTheDeal, dealFrom } from './dealing'
import { transitionsOn, withViewTransition } from './useViewTransition'

const LOG_LIMIT = 200
const FLIP_MS = 320

const socketUrl = (path: string) => {
  const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws'
  return `${protocol}://${window.location.host}${path}?token=${encodeURIComponent(token.value ?? '')}`
}

/** A game to play, or with `spectate` to watch: the same updates, read only. */
export const useGame = (gameId: string, spectate = false) => {
  const gamePath = `/api/games/${gameId}${spectate ? '/spectate' : ''}`
  const { t } = useI18n()
  let closed = false
  const error = ref<string | null>(null)
  const data = shallowRef<GameData | null>(null)
  const game = shallowRef<GameView | null>(null)
  const log = ref<string[]>([])
  const announcement = ref<string | null>(null)
  const flipping = ref<Set<string>>(new Set())

  let defs: Record<string, CardDef> = {}
  let pendingFlips: string[] = []
  let shuffledBack = new Set<string>()
  const queue: ServerMessage[] = []
  let draining = false
  let socket: WebSocket | null = null
  let reconnect: number | undefined

  const flip = async (cardId: string) => {
    flipping.value = new Set(flipping.value).add(cardId)
    await new Promise((resolve) => window.setTimeout(resolve, FLIP_MS))
    const remaining = new Set(flipping.value)
    remaining.delete(cardId)
    flipping.value = remaining
  }

  const update = async (next: GameData) => {
    data.value = next
    const view = toView(next, defs, game.value)
    // a game opening on the mulligan shows its empty hands first, so the opening hand deals out
    const undealt = game.value === null && transitionsOn() ? beforeTheDeal(view) : null
    if (undealt) {
      game.value = undealt
      await nextTick()
    }
    const dealing = transitionsOn() ? dealFrom(game.value, view, shuffledBack) : null
    shuffledBack = new Set()
    await withViewTransition(async () => {
      dealing?.lift()
      game.value = view
      await nextTick()
      dealing?.land()
    })
    dealing?.done()
    const flips = pendingFlips
    pendingFlips = []
    for (const cardId of flips) await flip(cardId)
  }

  // Log, announce and flip messages arrive while the server is working; the state that
  // follows them is applied in order, then any flips play on the new board.
  const apply = async (message: ServerMessage) => {
    switch (message.tag) {
      case 'Update':
        return update(message.contents)
      case 'Log':
        log.value = [...log.value, message.message].slice(-LOG_LIMIT)
        return
      case 'Announce':
        announcement.value = message.message
        return
      case 'FlipCard':
        pendingFlips.push(message.cardId)
        return
      case 'ShuffleIntoDeck':
        message.cardIds.forEach((id) => shuffledBack.add(id))
        return
    }
  }

  const drain = async () => {
    if (draining) return
    draining = true
    while (queue.length) await apply(queue.shift() as ServerMessage)
    draining = false
  }

  const post = async (path: string, body: unknown = {}) => {
    const response = await apiFetch(`/api/games/${gameId}${path}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    }).catch(() => null)
    error.value = response?.ok ? null : (await response?.text()) || t('game.errors.serverUnreachable')
  }

  const refresh = async () => {
    const response = await apiFetch(gamePath).catch(() => null)
    if (response?.ok) {
      queue.push({ tag: 'Update', contents: (await response.json()) as GameData })
      void drain()
    }
  }

  const connect = () => {
    socket = new WebSocket(socketUrl(gamePath))
    socket.addEventListener('open', () => {
      error.value = null
      void refresh()
    })
    socket.addEventListener('message', (event: MessageEvent<string>) => {
      queue.push(JSON.parse(event.data) as ServerMessage)
      void drain()
    })
    socket.addEventListener('close', () => {
      if (!closed) reconnect = window.setTimeout(connect, 2000)
    })
  }

  fetch('/api/cards')
    .then((response) => response.json())
    .then((cards: Record<string, CardDef>) => {
      defs = cards
      if (data.value) game.value = toView(data.value, defs, game.value)
    })
    .catch(() => {
      error.value = t('game.errors.cardData')
    })

  connect()

  onScopeDispose(() => {
    closed = true
    window.clearTimeout(reconnect)
    socket?.close()
  })

  return {
    error: readonly(error),
    game: readonly(game),
    canUndo: computed(() => data.value?.canUndo ?? false),
    log: readonly(log),
    announcement: readonly(announcement),
    flipping: readonly(flipping),
    started: computed(() => game.value?.started ?? false),
    question: computed(() => game.value?.question ?? null),
    scenario: computed(() => game.value?.scenario ?? null),
    players: computed(() => game.value?.players ?? []),
    table: computed(() => data.value?.table ?? null),
    outcome: computed(() => game.value?.outcome ?? null),
    undo: (to?: 'phase' | 'round') => post(to ? `/undo?to=${to}` : '/undo'),
    reset: () => {
      log.value = []
      announcement.value = null
      return post('/reset')
    },
    // answered as the seat it was asked of
    answer: (askId: number, choice: number, player: string) =>
      post('', { choice, questionVersion: askId, player }),
    debug: (message: object) => post('/debug', message),
    join: (deck: DeckChoice) => post('/join', { deck }),
    refresh,
  }
}
