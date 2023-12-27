type Card = {
  name: string;
}

type Hero = {
  card: Card
}

type Player = {
  heroes: Hero[];
  threat: number;
}

type Game = {
  players: Record<string, Player>;
}
