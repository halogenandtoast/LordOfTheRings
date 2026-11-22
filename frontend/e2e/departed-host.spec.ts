import { expect, test } from '@playwright/test'
import type { GameData, ServerGame } from '../src/server'

// The Long Defeat's response after its side quest is defeated: the quest has gone to the
// victory display, so the attachment's ability is a button that shows the card (issue 56)
test('an ability on an attachment whose host left play is a button showing the card', async () => {
  const { toView } = await import('../src/view')
  const host = { tag: 'QuestTarget', contents: 'side-quest' }
  const game: ServerGame = {
    players: { player: { id: 'player', name: 'Player', threat: 20, eliminated: false, hand: [], discard: [], deckSize: 0 } },
    playerOrder: ['player'], firstPlayer: 'player', phase: 'QuestPhase', round: 1, outcome: null,
    question: {
      player: {
        tag: 'QuestionLabel', label: '$label.useResponse', source: null, targets: [],
        question: {
          tag: 'ChooseOne',
          choices: [
            {
              tag: 'AbilityLabel', messages: [],
              ability: { abilityIndex: 1, abilitySource: { tag: 'AttachmentSource', contents: 'long-defeat' }, abilityType: { tag: 'ResponseAbility' } },
            },
            { tag: 'Label', label: '$label.pass', messages: [] },
          ],
        },
      },
    },
    questionVersion: 1,
    scenario: { name: 'Test', encounterDeckSize: 0, encounterDiscard: [], victoryDisplay: [], setAside: [] },
    entities: {
      heroes: {}, allies: {}, enemies: {}, locations: {}, quests: {}, treacheries: {}, objectives: {},
      attachments: { 'long-defeat': { id: 'long-defeat', cardId: 'card-long-defeat', cardCode: '10122', owner: 'player', placement: { tag: 'AttachedTo', contents: host } } },
    },
    activeCosts: {}, attachmentHosts: {}, committedWillpower: 0, stagingThreat: 0,
  } as unknown as ServerGame
  const data: GameData = { started: true, game, playerId: 'player', canUndo: true, log: null, table: { withFriends: false, seats: 1, owner: 1, seated: [] } }
  const [response] = toView(data, {}).question!.choices
  expect(response.targetId).toBeNull()
  expect(response.card?.id).toBe('card-long-defeat')
})
