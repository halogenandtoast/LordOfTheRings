import type { CardDef, Entity, GameData, Question, ServerGame, Target, UI } from './server'
import type { CardView, ChoiceView, GameView, PlayerView, QuestionView } from './types'

const cardTypes: Record<string, string> = {
  HeroType: 'hero',
  AllyType: 'ally',
  AttachmentType: 'attachment',
  EventType: 'event',
  EnemyType: 'enemy',
  LocationType: 'location',
  TreacheryType: 'treachery',
  QuestType: 'quest',
  ObjectiveType: 'objective',
  ObjectiveAllyType: 'objective-ally',
}

const phases: Record<string, string> = {
  ResourcePhase: 'resource',
  PlanningPhase: 'planning',
  QuestPhase: 'quest',
  TravelPhase: 'travel',
  EncounterPhase: 'encounter',
  CombatPhase: 'combat',
  RefreshPhase: 'refresh',
}

const targetKinds: Record<string, keyof ServerGame['entities']> = {
  HeroTarget: 'heroes',
  AllyTarget: 'allies',
  EnemyTarget: 'enemies',
  LocationTarget: 'locations',
  AttachmentTarget: 'attachments',
  QuestTarget: 'quests',
  TreacheryTarget: 'treacheries',
}

export const toView = (data: GameData, defs: Record<string, CardDef>): GameView => {
  const game = data.game
  if (!data.started || !game) return { started: false, round: 0, phase: null, outcome: null, over: false, question: null, scenario: null, players: [] }

  const def = (code: string): Partial<CardDef> => defs[code] ?? {}
  const entities = game.entities

  // The card an entity target refers to, so a choice can highlight it on the board.
  const targetCardId = (target: Target | undefined): string | null => {
    if (!target?.contents) return null
    if (target.tag === 'CardIdTarget') return target.contents
    const kind = targetKinds[target.tag]
    return kind ? entities[kind][target.contents]?.cardId ?? null : null
  }

  const targetName = (target: Target | undefined) => {
    const cardId = targetCardId(target)
    const entity = Object.values(entities).flatMap((es) => Object.values(es)).find((e) => e.cardId === cardId)
    return entity ? def(entity.cardCode).name ?? '' : ''
  }

  const card = (code: string, id: string, extra: Partial<CardView> = {}): CardView => {
    const d = def(code)
    return {
      id,
      code,
      name: d.name ?? code,
      type: cardTypes[d.cardType ?? ''] ?? 'unknown',
      flipped: false,
      threat: d.threat ?? d.threatCost ?? null,
      attack: d.attack ?? null,
      defense: d.defense ?? null,
      hitPoints: d.hitPoints ?? null,
      willpower: d.willpower ?? null,
      resources: null,
      damage: 0,
      exhausted: false,
      committed: false,
      progress: null,
      questPoints: d.questPoints ?? null,
      attachedTo: null,
      ...extra,
    }
  }

  const fromEntity = (e: Entity): CardView =>
    card(e.cardCode, e.cardId, {
      resources: e.tokens.Resource ?? null,
      damage: e.tokens.Damage ?? 0,
      progress: e.tokens.Progress ?? null,
      exhausted: e.exhausted ?? false,
      committed: e.committed ?? false,
      flipped: e.flipped ?? false,
      attachedTo: e.placement?.tag === 'AttachedTo' ? targetCardId(e.placement.contents as Target) : null,
    })

  const placed = (kind: keyof ServerGame['entities'], test: (e: Entity) => boolean) =>
    Object.values(entities[kind]).filter(test).map(fromEntity)

  const attachmentsOn = (cards: CardView[]) => {
    const ids = new Set(cards.map((c) => c.id))
    return placed('attachments', () => true).filter((a) => a.attachedTo !== null && ids.has(a.attachedTo))
  }

  const withAttachments = (cards: CardView[]) => [...cards, ...attachmentsOn(cards)]

  const isAt = (tag: string, contents?: string) => (e: Entity) =>
    e.placement?.tag === tag && (contents === undefined || e.placement.contents === contents)

  const players: PlayerView[] = game.playerOrder.map((pid) => {
    const p = game.players[pid]
    const characters = [
      ...placed('heroes', (h) => h.owner === pid),
      ...placed('allies', isAt('InPlayArea', pid)),
    ]
    return {
      playerId: pid,
      name: p.name,
      threat: p.threat,
      firstPlayer: game.firstPlayer === pid,
      eliminated: p.eliminated,
      handSize: p.hand.length,
      deckSize: p.deckSize,
      discardSize: p.discard.length,
      hand: p.hand.map((c) => card(c.cardCode, c.id)),
      discard: p.discard.map((c) => card(c.cardCode, c.id)),
      inPlay: withAttachments(characters),
      engaged: withAttachments(placed('enemies', isAt('EngagedWith', pid))),
    }
  })

  const staging = [...placed('enemies', isAt('StagingArea')), ...placed('locations', isAt('StagingArea'))]

  const toQuestion = (pid: string, question: Question): QuestionView => {
    let prompt = ''
    let q = question
    while (q.tag === 'QuestionLabel') {
      prompt = q.label
      q = q.question
    }
    const choices = q.choices.map((ui: UI, index): ChoiceView => {
      switch (ui.tag) {
        case 'Label':
          return { index, label: ui.label ?? '', targetId: null }
        case 'TargetLabel':
          return { index, label: targetName(ui.target), targetId: targetCardId(ui.target) }
        case 'AbilityLabel': {
          const kind = ui.ability?.abilityType.tag.replace(/Ability$/, '').replace(/^SilentForced$/, 'Forced') ?? 'Use'
          const source = ui.ability?.abilitySource
          return { index, label: `${kind}: ${targetName(source && { tag: source.tag.replace(/Source$/, 'Target'), contents: source.contents })}`, targetId: null }
        }
      }
    })
    return { askId: game.questionVersion, playerId: pid, prompt, choices }
  }

  const [asked] = Object.entries(game.question)

  return {
    started: true,
    round: game.round,
    phase: phases[game.phase] ?? game.phase,
    outcome: game.outcome === 'Won' ? 'WON' : game.outcome === 'Lost' ? 'LOST' : null,
    over: game.outcome !== null,
    question: asked ? toQuestion(asked[0], asked[1]) : null,
    scenario: {
      name: game.scenario.name,
      quest: placed('quests', () => true)[0] ?? null,
      activeLocation: placed('locations', isAt('ActiveLocation'))[0] ?? null,
      staging: withAttachments(staging),
      encounterDeck: game.scenario.encounterDeckSize,
      encounterDiscard: game.scenario.encounterDiscard.length,
      discard: game.scenario.encounterDiscard.map((c) => card(c.cardCode, c.id)),
    },
    players,
  }
}
