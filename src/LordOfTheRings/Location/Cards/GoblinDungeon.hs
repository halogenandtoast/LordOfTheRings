module LordOfTheRings.Location.Cards.GoblinDungeon (goblinDungeon) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))

newtype GoblinDungeon = GoblinDungeon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinDungeon :: LocationCard GoblinDungeon
goblinDungeon = location GoblinDungeon Cards.goblinDungeon

instance HasAbilities GoblinDungeon where
  getAbilities (GoblinDungeon a) = [mkAbility a 1 $ response $ ExploredWindow After a.id]

instance RunMessage GoblinDungeon where
  runMessage msg l@(GoblinDungeon a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      GoblinDungeon <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      searchDeckIntoPlay lead a 5 (CardWithType AllyType) 1 []
      pure l
    _ -> GoblinDungeon <$> runMessage msg a
