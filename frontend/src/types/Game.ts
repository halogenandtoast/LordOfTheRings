type Card = {
  name: string;
}

type Hero = {
  card: Card
  resources: number
}

type Player = {
  heroes: Hero[];
  threat: number;
  hand: Card[];
  deck: Card[];
}

type Game = {
  players: Record<string, Player>;
}
