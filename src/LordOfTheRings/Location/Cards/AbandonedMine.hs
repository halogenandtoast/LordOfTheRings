module LordOfTheRings.Location.Cards.AbandonedMine (abandonedMine) where

import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype AbandonedMine = AbandonedMine LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

abandonedMine :: LocationCard AbandonedMine
abandonedMine = location AbandonedMine Cards.abandonedMine

-- | Lost: Return the top 2 Goblin enemies in the encounter discard pile to the staging
-- area, if able.
instance RunMessage AbandonedMine where
  runMessage msg l@(AbandonedMine attrs) = case msg of
    TriggerLostEffects _ -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      traverse_ addToStagingFromDiscard $ take 2 $ filter (cardMatches $ CardWithTrait Goblin <> CardWithType EnemyType) pile
      pure l
    _ -> AbandonedMine <$> runMessage msg attrs
