module LordOfTheRings.Location.Cards.PoisonousShaft (poisonousShaft) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype PoisonousShaft = PoisonousShaft LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

poisonousShaft :: LocationCard PoisonousShaft
poisonousShaft = location PoisonousShaft Cards.poisonousShaft

instance HasModifiersFor PoisonousShaft where
  getModifiersFor (PoisonousShaft a) = when (a.placement == StagingArea) do
    characters <- filterM (fmap (> 0) . getDamage) =<< selectCharacters AnyHero AnyAlly
    for_ characters \character -> modified a character [CannotBeReadiedByPlayerCards]

instance HasAbilities PoisonousShaft where
  getAbilities (PoisonousShaft a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage PoisonousShaft where
  runMessage msg l@(PoisonousShaft a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> PoisonousShaft <$> runMessage msg a
