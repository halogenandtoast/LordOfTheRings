import { ikey } from './i18n'
import type { CardDef, Entity, GameData, Question, ServerGame, Target, UI } from './server'
import type { CardView, ChoiceView, GameView, PlayerView, QuestionView } from './types'

// ringsdb type names, as the board compares against them
const cardTypes: Record<string, string> = {
  HeroType: 'Hero',
  AllyType: 'Ally',
  AttachmentType: 'Attachment',
  EventType: 'Event',
  EnemyType: 'Enemy',
  LocationType: 'Location',
  TreacheryType: 'Treachery',
  QuestType: 'Quest',
  ObjectiveType: 'Objective',
  ObjectiveAllyType: 'Objective Ally',
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
  ObjectiveTarget: 'objectives',
}
const kindTargets = Object.fromEntries(Object.entries(targetKinds).map(([tag, kind]) => [kind, tag]))

// Cards stay where they were in hand, new ones going on the end.
const keepOrder = (previous: CardView[] | undefined, cards: CardView[]) => {
  const rank = new Map(previous?.map((c, i) => [c.id, i]))
  return [...cards].sort((a, b) => (rank.get(a.id) ?? Number.MAX_SAFE_INTEGER) - (rank.get(b.id) ?? Number.MAX_SAFE_INTEGER))
}

export const toView = (data: GameData, defs: Record<string, CardDef>, previous: GameView | null = null): GameView => {
  const game = data.game
  if (!data.started || !game) return { started: false, round: 0, phase: null, outcome: null, over: false, question: null, attachmentHosts: {}, scenario: null, players: [], tally: null }

  const def = (code: string): Partial<CardDef> => defs[code] ?? {}
  const entities = game.entities

  // The card an entity target refers to, so a choice can highlight it on the board.
  const targetCardId = (target: Target | undefined): string | null => {
    if (!target?.contents) return null
    if (target.tag === 'CardIdTarget') return target.contents
    const kind = targetKinds[target.tag]
    return kind ? entities[kind]?.[target.contents]?.cardId ?? null : null
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
      type: cardTypes[d.cardType ?? ''] ?? 'Unknown',
      flipped: false,
      threat: d.threat ?? d.threatCost ?? null,
      attack: d.attack ?? null,
      defense: d.defense ?? null,
      hitPoints: game.hitPoints?.[id] ?? d.hitPoints ?? null,
      willpower: d.willpower ?? null,
      resources: null,
      damage: 0,
      exhausted: false,
      committed: false,
      progress: null,
      questPoints: d.questPoints ?? null,
      spheres: d.sphere ? [d.sphere] : [],
      attachedTo: null,
      target: null,
      playing: false,
      stats: game.stats?.[id] ?? [],
      attackStrength: game.attackStrength?.[id],
      ...extra,
    }
  }

  // resource icons gained from modifiers, such as Celebrían's Stone on Aragorn
  const gainedIcons = (target: Target) =>
    (game.modifiers ?? [])
      .filter(([t]) => t.tag === target.tag && t.contents === target.contents)
      .flatMap(([, modifiers]) => modifiers)
      .flatMap((m) => (m.modifierType.tag === 'GainResourceIcon' ? [m.modifierType.contents as string] : []))

  const fromEntity = (e: Entity, kind: keyof ServerGame['entities']): CardView => {
    const target = { tag: kindTargets[kind], contents: e.id }
    const d = def(e.cardCode)
    return card(e.cardCode, e.cardId, {
      spheres: [...new Set([...(d.sphere ? [d.sphere] : []), ...gainedIcons(target)])],
      target,
      resources: e.tokens.Resource ?? null,
      damage: e.tokens.Damage ?? 0,
      progress: e.tokens.Progress ?? null,
      exhausted: e.exhausted ?? false,
      committed: e.committed ?? false,
      flipped: e.flipped ?? false,
      facedown: (e.prisoner ?? false) || e.cardCode === 'OrcGuard' ? 'player' : undefined,
      attachedTo: e.placement?.tag === 'AttachedTo' ? targetCardId(e.placement.contents as Target) : null,
    })
  }

  // shadow cards sit under the enemy they were dealt to, face down until they resolve
  const shadowsOf = (pid: string) =>
    Object.values(entities.enemies)
      .filter(isAt('EngagedWith', pid))
      .flatMap((e) =>
        (e.shadowCards ?? []).map((shadow) => {
          const revealed = (e.shadowRevealed ?? []).includes(shadow.id)
          return card(shadow.cardCode, shadow.id, {
            attachedTo: e.cardId,
            ...(revealed ? {} : { facedown: 'encounter' as const, name: ikey('log.shadowCard') }),
          })
        }),
      )

  const placed = (kind: keyof ServerGame['entities'], test: (e: Entity) => boolean) =>
    // games saved before a kind existed do not have it
    Object.values(entities[kind] ?? {}).filter(test).map((e) => fromEntity(e, kind))

  const attachmentsOn = (cards: CardView[]) => {
    const ids = new Set(cards.map((c) => c.id))
    // claimed objectives and condition treacheries sit on their hosts like attachments
    return [...placed('attachments', () => true), ...placed('objectives', () => true), ...placed('treacheries', () => true)].filter(
      (a) => a.attachedTo !== null && ids.has(a.attachedTo),
    )
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
      handSize: p.handSize ?? p.hand.length,
      deckSize: p.deckSize,
      discardSize: p.discard.length,
      hand: keepOrder(previous?.players.find((v) => v.playerId === pid)?.hand, [
        ...p.hand.map((c) => card(c.cardCode, c.id)),
        // played but not yet in play: being paid for, or an attachment waiting for its host
        ...Object.values(game.activeCosts ?? {}).filter((cost) => cost.player === pid).map((cost) => card(cost.card.cardCode, cost.card.id, { playing: true })),
        ...Object.values(entities.attachments).filter((a) => a.owner === pid && a.placement?.tag === 'Unplaced').map((a) => card(a.cardCode, a.cardId, { playing: true })),
      ]),
      discard: p.discard.map((c) => card(c.cardCode, c.id)),
      inPlay: withAttachments(characters),
      engaged: [...withAttachments(placed('enemies', isAt('EngagedWith', pid))), ...shadowsOf(pid)],
    }
  })

  const staging = [
    ...placed('enemies', isAt('StagingArea')),
    ...placed('locations', isAt('StagingArea')),
    ...placed('objectives', isAt('StagingArea')),
    // a treachery shows in staging while it resolves, until it attaches or is discarded
    ...placed('treacheries', isAt('Unplaced')),
  ]

  const toQuestion = (pid: string, question: Question): QuestionView => {
    let prompt = ''
    let sourceId: string | null = null
    let targetIds: string[] = []
    let searched: CardView[] | null = null
    let q = question
    while (q.tag === 'QuestionLabel' || q.tag === 'SearchCards' || q.tag === 'PlayerWindow') {
      if (q.tag === 'SearchCards') {
        searched = q.cards.map((c) => card(c.cardCode, c.id))
      } else if (q.tag === 'QuestionLabel') {
        prompt = q.label
        sourceId = targetCardId(q.source ?? undefined)
        targetIds = q.targets.map(targetCardId).filter((id): id is string => id !== null)
      }
      q = q.question
    }
    const choices = q.choices.map((ui: UI, index): ChoiceView => {
      switch (ui.tag) {
        case 'Label':
          return { index, label: ui.label ?? '', targetId: null }
        case 'TargetLabel':
          return { index, label: targetName(ui.target), targetId: targetCardId(ui.target) }
        case 'AbilityLabel': {
          const kind = ui.ability?.abilityType.tag.replace(/Ability$/, '').replace(/^SilentForced$/, 'Forced') ?? 'Action'
          const source = ui.ability?.abilitySource
          const sourceCard = source && { tag: source.tag.replace(/Source$/, 'Target'), contents: source.contents }
          // the ability's own card is what you click to use it
          return { index, label: ikey(`label.ability.${kind}`, { name: targetName(sourceCard) }), targetId: targetCardId(sourceCard) }
        }
      }
    })
    return { askId: game.questionVersion, playerId: pid, prompt, sourceId, targetIds, choices, searched }
  }

  const [asked] = Object.entries(game.question)
  // the active location's threat no longer counts toward the staging area's
  const active = placed('locations', isAt('ActiveLocation'))[0]
  const activeLocation = active ? { ...active, stats: active.stats.filter(([stat]) => stat !== 'threat') } : null

  return {
    started: true,
    round: game.round,
    phase: phases[game.phase] ?? game.phase,
    outcome: game.outcome === 'Won' ? 'WON' : game.outcome === 'Lost' ? 'LOST' : null,
    over: game.outcome !== null,
    question: asked ? toQuestion(asked[0], asked[1]) : null,
    attachmentHosts: Object.fromEntries(
      Object.entries(game.attachmentHosts ?? {}).map(([cardId, hosts]) => [
        cardId,
        hosts.map(targetCardId).filter((id): id is string => id !== null),
      ]),
    ),
    scenario: {
      name: game.scenario.name,
      quest: placed('quests', () => true)[0] ?? null,
      activeLocation,
      staging: withAttachments(staging),
      encounterDeck: game.scenario.encounterDeckSize,
      encounterDiscard: game.scenario.encounterDiscard.length,
      discard: game.scenario.encounterDiscard.map((c) => card(c.cardCode, c.id)),
      victory: game.scenario.victoryDisplay.map((c) => card(c.cardCode, c.id)),
    },
    players,
    tally: { willpower: game.committedWillpower, threat: game.stagingThreat, lastWillpower: game.lastCommittedWillpower },
  }
}
