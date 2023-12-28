type Card = {
  name: string;
}

type Hero = {
  card: Card
  resources: number
}

type Choice = { tag: "CardChoice", contents: string }
type Question = { tag: "ChooseOne", contents: Choice[] }

type Player = {
  heroes: Hero[];
  threat: number;
  hand: Card[];
  deck: Card[];
  question: Question | null;
}

type Game = {
  players: Record<string, Player>;
}

type GameData = {
  game: Game;
  playerId: string;
}
