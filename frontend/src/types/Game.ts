type Card = {
  name: string;
}

type Hero = {
  card: Card
  resources: number
  player: string
}

type Choice = { tag: "CardChoice", contents: string }
type Question = { tag: "ChooseOne", contents: Choice[] }

type Player = {
  threat: number;
  hand: Card[];
  deck: Card[];
  question: Question | null;
}

type Placement = { tag: "Controlled", contents: string }

type Ally = {
  card: Card;
  placement: Placement
}

type Entities = {
  heroes: Record<string, Hero>;
  allies: Record<string, Ally>;
}

type Game = {
  players: Record<string, Player>;
  entities: Entities;
}

type GameData = {
  game: Game;
  playerId: string;
}
