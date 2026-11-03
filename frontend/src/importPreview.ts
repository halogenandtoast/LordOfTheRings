import type { CardDef, GameSummary, ServerGame } from './server'

/** Read an export locally; selecting a file must not create a game. */
export const importPreview = (value: unknown, cards: Record<string, CardDef> = {}): GameSummary => {
  const exported = value as { version: number; name: string; playerName: string; game: ServerGame }
  const game = exported?.game
  if (exported?.version !== 1 || typeof exported.name !== 'string' || typeof exported.playerName !== 'string'
    || !game || typeof game.scenario?.name !== 'string' || !Array.isArray(game.playerOrder)
    || typeof game.round !== 'number' || typeof game.phase !== 'string'
    || !game.players || !game.entities?.heroes) throw new Error('Invalid game export')

  const heroes = Object.values(game.entities.heroes)
  return {
    id: '',
    name: exported.name,
    playerName: exported.playerName,
    scenario: game.scenario.name,
    round: game.round,
    phase: game.phase,
    threat: game.players[game.playerOrder[0] ?? '']?.threat ?? 0,
    outcome: game.outcome,
    victoryPoints: (game.scenario.victoryDisplay ?? []).reduce((total, card) => total + (cards[card.cardCode]?.victoryPoints ?? 0), 0),
    score: game.outcome === 'Won' ? game.score?.total ?? null : null,
    waiting: false,
    easyMode: game.scenario.mode === 'EasyMode' || ((game.scenario as ServerGame['scenario'] & { easyMode?: boolean }).easyMode ?? false),
    nightmare: game.scenario.mode === 'NightmareMode',
    seats: game.playerOrder.map((id) => {
      const player = game.players[id]!
      return {
        name: player.name,
        threat: player.threat,
        eliminated: player.eliminated,
        heroes: heroes.filter((hero) => hero.owner === id).map((hero) => ({
          code: hero.cardCode,
          sphere: cards[hero.cardCode]?.sphere ?? null,
        })),
      }
    }),
  }
}
