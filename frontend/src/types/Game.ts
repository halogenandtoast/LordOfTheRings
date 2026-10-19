type Card = {
  name: string;
}

type Hero = {
  card: Card
  resources: number
  player: string
}

type Quest = {
  id: string
}

type Choice = { tag: "CardChoice", contents: string }
type Question = { tag: "ChooseOne", contents: Choice[] }

type Player = {
  threat: number;
  hand: Card[];
  deck: Card[];
  question: Question | null;
}

type Placement = { tag: "InPlayArea", contents: string }

type Ally = {
  card: Card;
  placement: Placement
}

type Enemy = {
  card: Card;
  placement: Placement
}

type Location = {
  card: Card;
  placement: Placement
  progress: number
}

type Entities = {
  heroes: Record<string, Hero>;
  allies: Record<string, Ally>;
  enemies: Record<string, Enemy>;
  locations: Record<string, Location>;
}

type Scenario = {
  quest: Card[]
  encounterDeck: Card[]
}

type Game = {
  players: Record<string, Player>;
  entities: Entities;
  scenario: Scenario;
}

type GameData = {
  game: Game;
  playerId: string;
}
