import { ikey } from './i18n'
import type { Card, CardDef, Entity, GameData, Message, Question, ServerGame, Target, UI } from './server'
import type { CardView, ChoiceView, GameView, PlayerView, QuestionView, SearchGroup } from './types'

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
  CampaignType: 'Campaign',
  PlayerSideQuestType: 'Player Side Quest',
  EncounterSideQuestType: 'Encounter Side Quest',
  NightmareType: 'Nightmare',
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
  // a side quest, a player's or one revealed from the encounter deck, waits in staging
  const isSideQuest = (e: Entity) => ['PlayerSideQuestType', 'EncounterSideQuestType'].includes(def(e.cardCode).cardType ?? '')
  const threatening = new Set(game.threatening ?? [])
  const entities = game.entities

  // The card an entity target refers to, so a choice can highlight it on the board.
  const targetCardId = (target: Target | undefined): string | null => {
    if (!target?.contents) return null
    if (target.tag === 'CardIdTarget') return target.contents
    const kind = targetKinds[target.tag]
    // an ally that just left play is still named, as its own response is asked
    const departed = target.tag === 'AllyTarget' ? game.leftPlay?.[target.contents] : undefined
    return kind ? entities[kind]?.[target.contents]?.cardId ?? departed?.cardId ?? null : null
  }

  const targetName = (target: Target | undefined) => {
    if (target?.tag === 'PlayerTarget') return game.players[target.contents ?? '']?.name ?? ''
    const cardId = targetCardId(target)
    const entity = [...Object.values(entities), game.leftPlay ?? {}].flatMap((es) => Object.values(es)).find((e) => e.cardId === cardId)
    return entity ? def(entity.cardCode).name ?? '' : ''
  }

  // the cards of a player playing them as printed, by card id: every other card, the
  // encounter deck's included, shows its errata
  const printed = new Set<string>()
  for (const p of Object.values(game.players).filter((p) => p.errata === false)) {
    for (const c of [...(p.hand ?? []), ...p.discard]) printed.add(c.id)
    for (const es of Object.values(entities)) for (const e of Object.values(es ?? {})) if (e.owner === p.id) printed.add(e.cardId)
    for (const cost of Object.values(game.activeCosts ?? {})) if (cost.player === p.id) printed.add(cost.card.id)
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
      counting: [],
      attackStrength: game.attackStrength?.[id],
      errata: !printed.has(id),
      ...extra,
    }
  }

  // resource icons gained from modifiers, such as Celebrían's Stone on Aragorn
  const gainedIcons = (target: Target) =>
    (game.modifiers ?? [])
      .filter(([t]) => t.tag === target.tag && t.contents === target.contents)
      .flatMap(([, modifiers]) => modifiers)
      .flatMap((m) => (m.modifierType.tag === 'GainResourceIcon' ? [m.modifierType.contents as string] : []))

  // considered out of play while on the table, as the card Saruman chooses
  const heldOutOfPlay = (target: Target) =>
    (game.modifiers ?? []).some(
      ([t, modifiers]) => t.tag === target.tag && t.contents === target.contents && modifiers.some((m) => m.modifierType.tag === 'ConsideredOutOfPlay'),
    )

  // a Grapple enemy lies facedown on the location it grapples (The Thing in the Depths)
  const grappled = (e: Entity, kind: keyof ServerGame['entities']) =>
    kind === 'enemies' && e.placement?.tag === 'AttachedTo' && (e.placement.contents as Target)?.tag === 'LocationTarget'

  // a guarded objective is attached to the card guarding it for as long as that card is in play
  const guardInPlay = (guard: string | null | undefined) =>
    guard && [...Object.values(entities.enemies), ...Object.values(entities.locations), ...Object.values(entities.treacheries ?? {})].some((e) => e.cardId === guard) ? guard : null

  const fromEntity = (e: Entity, kind: keyof ServerGame['entities']): CardView => {
    const target = { tag: kindTargets[kind], contents: e.id }
    const d = def(e.cardCode)
    return card(e.cardCode, e.cardId, {
      spheres: [...new Set([...(d.sphere ? [d.sphere] : []), ...gainedIcons(target)])],
      target,
      // villagers are resource tokens on the table
      resources: e.tokens.Resource ?? e.tokens.Villager ?? null,
      time: e.tokens.TimeCounter ?? null,
      damage: e.tokens.Damage ?? 0,
      progress: e.tokens.Progress ?? null,
      exhausted: e.exhausted ?? false,
      committed: e.committed ?? false,
      outOfPlay: heldOutOfPlay(target),
      flipped: e.flipped ?? false,
      otherSide: e.otherSide ?? undefined,
      facedown: (e.prisoner ?? false) || e.cardCode === 'OrcGuard' || e.cardCode === 'ReanimatedDead' ? 'player' : grappled(e, kind) ? 'encounter' : undefined,
      ...(grappled(e, kind) ? { name: ikey('log.grappled') } : {}),
      attachedTo: e.placement?.tag === 'AttachedTo' ? targetCardId(e.placement.contents as Target) : guardInPlay(e.guard),
      counting: [...(e.committed ? ['willpower'] : []), ...(threatening.has(e.cardId) ? ['threat'] : [])],
    })
  }

  // shadow cards sit under the enemy they were dealt to, face down until they resolve
  const shadowsOf = (pid: string) =>
    Object.values(entities.enemies)
      .filter((e) => isAt('EngagedWith', pid)(e) || isAt('AttackingFromShadow', pid)(e))
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

  // the cards facedown under a card lie under it as one stack, counted, as the underworld
  // cards under a location (The Steward's Fear) and those under The Orcs' Search
  const stackUnder = (host: CardView) => {
    const count = game.underneath?.[host.id] ?? 0
    // and the player cards captured under it (Escape from Mount Gram)
    const captured = game.captured?.[host.id] ?? 0
    return [
      ...(count ? [card('', `underneath-${host.id}`, { facedown: 'encounter', name: ikey('log.cardsUnderneath', { count }), attachedTo: host.id })] : []),
      ...(captured ? [card('', `captured-${host.id}`, { facedown: 'player', name: ikey('log.capturedUnderneath', { count: captured }), attachedTo: host.id })] : []),
    ]
  }

  const attachmentsOn = (cards: CardView[]) => {
    const ids = new Set(cards.map((c) => c.id))
    // claimed objectives, condition treacheries and enemies attached as Grasping Tentacle is sit on their hosts like attachments
    // and allies under a quest card, as Captured! holds Faramir, and guarded locations on their guards (Across the Ettenmoors)
    return [
      ...[...placed('attachments', () => true), ...placed('objectives', () => true), ...placed('treacheries', () => true), ...placed('enemies', () => true), ...placed('allies', () => true), ...placed('locations', isAt('AttachedTo'))].filter(
        (a) => a.attachedTo !== null && ids.has(a.attachedTo),
      ),
      ...cards.flatMap(stackUnder),
    ]
  }

  const withAttachments = (cards: CardView[]) => [...cards, ...attachmentsOn(cards)]

  const isAt = (tag: string, contents?: string) => (e: Entity) =>
    e.placement?.tag === tag && (contents === undefined || e.placement.contents === contents)

  const players: PlayerView[] = game.playerOrder.map((pid) => {
    const p = game.players[pid]
    const onPlayer = (e: Entity) => e.placement?.tag === 'AttachedTo' && (e.placement.contents as Target)?.tag === 'PlayerTarget' && (e.placement.contents as Target).contents === pid
    const characters = [
      ...placed('heroes', (h) => h.owner === pid),
      ...placed('allies', isAt('InPlayArea', pid)),
      // and the locations they control, as Osgiliath's
      ...placed('locations', isAt('InPlayArea', pid)),
    ]
    return {
      playerId: pid,
      name: p.name,
      threat: p.threat,
      firstPlayer: game.firstPlayer === pid,
      eliminated: p.eliminated,
      handSize: p.handSize ?? p.hand.length,
      deckSize: p.deckSize,
      capturedDeckSize: game.capturedDecks?.[pid] ?? 0,
      discardSize: p.discard.length,
      hand: keepOrder(previous?.players.find((v) => v.playerId === pid)?.hand, [
        ...p.hand.map((c) => card(c.cardCode, c.id, { prohibited: game.playProhibitions?.[c.id] })),
        // played but not yet in play: being paid for, or an attachment waiting for its host
        ...Object.values(game.activeCosts ?? {}).filter((cost) => cost.player === pid).map((cost) => card(cost.card.cardCode, cost.card.id, { playing: true })),
        ...Object.values(entities.attachments).filter((a) => a.owner === pid && a.placement?.tag === 'Unplaced').map((a) => card(a.cardCode, a.cardId, { playing: true })),
      ]),
      discard: p.discard.map((c) => card(c.cardCode, c.id)),
      // a condition on top of their deck, as Iron Shackles
      onDeck: placed('treacheries', onPlayer),
      // hidden cards lie facedown in the player's play area (The Blood of Gondor)
      inPlay: [
        ...withAttachments(characters),
        // an attachment on the player's threat dial, as Favor of the Valar
        ...placed('attachments', onPlayer),
        ...(game.scenario.hiddenCards?.[pid] ?? []).map((id) => card('', id, { facedown: 'encounter', name: ikey('log.hiddenCard') })),
      ],
      engaged: [
        ...withAttachments(placed('enemies', isAt('EngagedWith', pid))),
        ...withAttachments(placed('enemies', isAt('AttackingFromShadow', pid))),
        ...shadowsOf(pid),
      ],
    }
  })

  // separated, the board's scenario is the area in focus, and the other areas are shown
  // beside it, each with its own quest, active location and staging area
  const areas = game.areas ?? []
  const inArea = (cards: string[] | undefined) => {
    const ids = new Set(cards ?? [])
    return (c: CardView) => areas.length === 0 || ids.has(c.id)
  }
  const inFocus = inArea(areas.find((a) => a.focused)?.cards)

  // the Island Map's locations stay in their places (Temple of the Deceived): shown there,
  // not in the staging area or the active location's slot
  const offMap = (e: Entity) => !e.position
  const allStaging = [
    ...placed('enemies', isAt('StagingArea')),
    ...placed('locations', (e) => isAt('StagingArea')(e) && offMap(e)),
    // a guarded one comes in with its guard's attachments, wherever the guard is
    ...placed('objectives', isAt('StagingArea')).filter((o) => o.attachedTo === null),
    // a Trap waits in staging until an enemy walks into it
    ...placed('attachments', isAt('StagingArea')),
    // a treachery shows in staging while it resolves, until it attaches or is discarded,
    // and stays there when it is added to it, as Blinding Blizzard
    ...placed('treacheries', (e) => isAt('Unplaced')(e) || isAt('StagingArea')(e)),
    // a side quest waits in staging until it is defeated
    ...placed('quests', isSideQuest),
  ]
  const staging = allStaging.filter(inFocus)

  const damageTo = (id: string | null, messages: Message[]): number =>
    messages.reduce((sum, m) => {
      if (m.tag === 'DealDamage') {
        const [, target, n] = m.contents as [unknown, Target, number]
        return sum + (id && targetCardId(target) === id ? n : 0)
      }
      return sum + (m.tag === 'Run' ? damageTo(id, m.contents as Message[]) : 0)
    }, 0)

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
    // Saved setup questions from before searches carried their cards still need a
    // picker: their targets are in the encounter deck, outside the visible board.
    if (searched === null && q.choices.length && q.choices.every((ui) => ui.tag === 'TargetLabel' && ui.target?.tag === 'CardIdTarget')) {
      const found = q.choices.map((ui) => {
        const message = ui.messages?.find((m) => m.tag === 'AddToStaging')
        const c = message?.contents as Partial<Card> | undefined
        return c && typeof c.id === 'string' && typeof c.cardCode === 'string' && c.id === ui.target?.contents
          ? card(c.cardCode, c.id) : null
      })
      if (found.every((c) => c !== null)) {
        searched = found
        if (!prompt) prompt = ikey('label.searchChoose')
      }
    }
    const choices = q.choices.map((ui: UI, index): ChoiceView => {
      switch (ui.tag) {
        case 'Label':
          return { index, label: ui.label ?? '', targetId: null, action: ui.label ?? '' }
        case 'TargetLabel':
          return { index, label: targetName(ui.target), targetId: targetCardId(ui.target), action: prompt, damage: damageTo(targetCardId(ui.target), ui.messages ?? []) }
        case 'AbilityLabel': {
          const kind = ui.ability?.abilityType.tag.replace(/Ability$/, '').replace(/^SilentForced$/, 'Forced') ?? 'Action'
          const source = ui.ability?.abilitySource
          const sourceCard = source && { tag: source.tag.replace(/Source$/, 'Target'), contents: source.contents }
          // the ability's own card is what you click to use it, but an ally that left play
          // for a deck, as Éomund shuffled away by Lothíriel, is out of sight: it is a button
          // that shows the card
          const label = ikey(`label.ability.${kind}`, { name: targetName(sourceCard) })
          const cardId = targetCardId(sourceCard)
          const sourceEntity = sourceCard && sourceCard.tag in targetKinds
            ? entities[targetKinds[sourceCard.tag]]?.[sourceCard.contents ?? ''] : undefined
          const tooltip = sourceEntity?.cardCode === '02095' && [1, 2].includes(ui.ability?.abilityIndex ?? 0)
            ? ikey(`label.boromir.action${ui.ability!.abilityIndex}`) : undefined
          const departed = source?.tag === 'AllySource' && source.contents && game.leftPlay?.[source.contents]
          const seen = Object.values(game.players).some((p) => [...(p.hand ?? []), ...p.discard].some((c) => c.id === cardId))
          if (departed && !seen) return { index, label, targetId: null, action: label, card: card(departed.cardCode, departed.cardId) }
          return { index, label, targetId: cardId, action: label, tooltip }
        }
      }
    })
    const amount = q.tag === 'ChooseAmount' ? { min: q.minAmount, max: q.minAmount + q.choices.length - 1 } : null
    // a search shows its cards by the zone each lies in, the zones in the order they first come
    const zoned = searched?.reduce<SearchGroup[]>((groups, c) => {
      const zone = game.searchZones?.[c.id] ?? { zone: 'other' }
      const player = zone.player ? (game.players[zone.player]?.name ?? null) : null
      const group = groups.find((g) => g.zone === zone.zone && g.player === player)
      if (group) group.cards.push(c)
      else groups.push({ zone: zone.zone, player, cards: [c] })
      return groups
    }, []) ?? null
    return { askId: game.questionVersion, playerId: pid, prompt, sourceId, targetIds, choices, searched: zoned, amount }
  }

  const activeLocations = placed('locations', (e) => isAt('ActiveLocation')(e) && offMap(e))
  const islandMap = Object.values(entities.locations ?? {}).flatMap((e) => {
    if (!e.position) return []
    const card = fromEntity(e, 'locations')
    return [{ card, attachments: attachmentsOn([card]), ...e.position, active: isAt('ActiveLocation')(e), staged: isAt('StagingArea')(e) }]
  })
  const quests = placed('quests', (q) => !isSideQuest(q))
  const [activeLocation, ...otherActiveLocations] = activeLocations.filter(inFocus)
  const [quest] = quests.filter(inFocus)
  const otherAreas = areas
    .filter((a) => !a.focused)
    .map((a) => {
      const here = inArea(a.cards)
      return {
        players: a.players.map((pid) => game.players[pid]?.name ?? pid),
        quest: quests.find(here) ?? null,
        activeLocation: activeLocations.find(here) ?? null,
        staging: withAttachments(allStaging.filter(here)),
        threat: a.threat,
      }
    })

  const [asked] = Object.entries(game.question)

  return {
    started: true,
    round: game.round,
    phase: phases[game.phase] ?? game.phase,
    score: game.score,
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
      quest: quest ?? null,
      questAttachments: attachmentsOn(quest ? [quest] : []),
      activeLocation: activeLocation ?? null,
      activeLocationAttachments: attachmentsOn(activeLocation ? [activeLocation] : []),
      otherActiveLocations: otherActiveLocations.map((card) => ({ card, attachments: attachmentsOn([card]) })),
      islandMap,
      staging: withAttachments(staging),
      otherAreas,
      encounterDeck: game.scenario.encounterDeckSize,
      encounterDiscard: game.scenario.encounterDiscard.length,
      discard: game.scenario.encounterDiscard.map((c) => card(c.cardCode, c.id)),
      victory: game.scenario.victoryDisplay.map((c) => card(c.cardCode, c.id)),
      setAside: game.scenario.setAside.map((c) => card(c.cardCode, c.id)),
      victoryPoints: game.scenario.victoryDisplay.reduce((total, c) => total + (def(c.cardCode).victoryPoints ?? 0), 0),
      heading: game.scenario.heading?.toLowerCase() ?? null,
      mode: game.scenario.mode ?? 'NormalMode',
      // its image is the flipped side, the one kept beside the quest deck
      nightmareCard:
        game.scenario.mode === 'NightmareMode' && game.scenario.nightmareDeck
          ? card(game.scenario.nightmareDeck.setupCard, 'nightmare-setup')
          : null,
      corsairDeck: game.scenario.corsairDeckSize ?? null,
      corsairDiscard: (game.scenario.corsairDiscard ?? []).map((c) => card(c.cardCode, c.id)),
      campaignCard: game.campaignCard ? card(game.campaignCard.code, `campaign-${game.campaignCard.code}`, { flipped: game.campaignCard.flipped }) : null,
      stormcallerStage: game.stormcallerStage
        ? card(game.stormcallerStage.cardCode, game.stormcallerStage.id, { flipped: true, image: `${game.stormcallerStage.cardCode}-d`, progress: game.stormcallerProgress ?? 0 })
        : null,
      stormcallerArea: withAttachments([...placed('enemies', isAt('StormcallerArea')), ...placed('locations', isAt('StormcallerArea'))]),
      orcsStage: game.orcsStage
        ? card(game.orcsStage.cardCode, game.orcsStage.id, { flipped: true, image: `${game.orcsStage.cardCode}-d`, progress: game.orcsProgress ?? 0 })
        : null,
      orcsArea: withAttachments([...placed('enemies', isAt('OrcsArea')), ...placed('locations', isAt('OrcsArea'))]),
    },
    players,
    tally: { willpower: game.committedWillpower, threat: game.stagingThreat, lastWillpower: game.lastCommittedWillpower },
  }
}
