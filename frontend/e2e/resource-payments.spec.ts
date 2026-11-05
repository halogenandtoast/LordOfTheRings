import { expect, test } from '@playwright/test'
import { pendingResourcePayments } from '../src/resourcePayments'
import type { Message, Question } from '../src/server'

const question = (messages: Message[]): Question => ({
  tag: 'QuestionLabel', label: 'Pay more', source: null, targets: [],
  question: { tag: 'ChooseOne', choices: messages.map(message => ({ tag: 'TargetLabel', messages: [message] })) },
})
const payment = (next: string, chosen: string[], group = false): Message => ({
  tag: 'SpendResourcesChosen', contents: ['player', group, 'Neutral', 2, [next, ...chosen]],
})

test('shows only allocated resources, including repeated payments from a hero', () => {
  expect([...pendingResourcePayments({ player: question([payment('a', []), payment('b', [])]) })]).toEqual([])
  expect([...pendingResourcePayments({ player: question([payment('a', ['a', 'a', 'b']), payment('b', ['a', 'a', 'b'])]) })]).toEqual([['a', 2], ['b', 1]])
})

test('supports group payments and payments from distinct heroes', () => {
  expect([...pendingResourcePayments({ player: question([payment('b', ['a'], true)]) })]).toEqual([['a', 1]])
  expect([...pendingResourcePayments({ player: question([{ tag: 'SpendResourcesFromDistinctHeroes', contents: ['player', 'Neutral', 1, ['b', 'a']] }]) })]).toEqual([['a', 1]])
})

test('clears the preview when payment completes or is undone', () => {
  expect([...pendingResourcePayments({})]).toEqual([])
  expect([...pendingResourcePayments({ player: question([payment('a', [])]) })]).toEqual([])
  expect([...pendingResourcePayments({ player: question([{ tag: 'SpendResourcesFrom', contents: ['player', [['a', 2]]] }]) })]).toEqual([])
})

test('board shows remaining hero resources and does not deduct twice on completion', async () => {
  const { toView } = await import('../src/view')
  const game: import('../src/server').ServerGame = {
    players: { player: { id: 'player', name: 'Player', threat: 20, eliminated: false, hand: [], discard: [], deckSize: 0 } },
    playerOrder: ['player'], firstPlayer: 'player', phase: 'PlanningPhase', round: 1, outcome: null,
    question: { player: question([payment('b', ['a', 'a'])]) }, questionVersion: 1,
    scenario: { name: 'Test', encounterDeckSize: 0, encounterDiscard: [], victoryDisplay: [], setAside: [] },
    entities: { heroes: {
      a: { id: 'a', cardId: 'card-a', cardCode: '01001', owner: 'player', tokens: { Resource: 3 } },
      b: { id: 'b', cardId: 'card-b', cardCode: '01002', owner: 'player', tokens: { Resource: 2 } },
    }, allies: {}, enemies: {}, locations: {}, attachments: {}, quests: {}, treacheries: {}, objectives: {} },
    activeCosts: {}, attachmentHosts: {}, committedWillpower: 0, stagingThreat: 0,
  }
  const data: import('../src/server').GameData = { started: true, game, playerId: 'player', canUndo: true, log: null, table: { withFriends: false, seats: 1, owner: 1, seated: [] } }
  const resources = () => toView(data, {}).players[0].inPlay.map(card => card.resources)
  expect(resources()).toEqual([1, 2])
  expect(game.entities.heroes.a.tokens.Resource).toBe(3)
  game.question = {}
  game.entities.heroes.a.tokens.Resource = 1
  expect(resources()).toEqual([1, 2])
})
