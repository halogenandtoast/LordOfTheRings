module LordOfTheRings.Quest.Cards.FindWater (findWater) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FindWater = FindWater QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Desert Sands).
findWater :: QuestCard FindWater
findWater = quest FindWater Cards.findWater

-- | Characters cannot be readied by player card effects.
instance HasModifiersFor FindWater where
  getModifiersFor (FindWater a) = modified a GameTarget [CannotBeReadiedByPlayerCards]

-- | Response: When this stage is defeated, each player readies 1 hero he controls. The
-- players gain by it, so it is taken, as Make Camp's is. Find Water's own effect is no
-- player card's, so its bar, still in play as it resolves, does not stop it.
instance RunMessage FindWater where
  runMessage msg (FindWater attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy (PlayerWithId p) <> ExhaustedHero)
        chooseOrRunOneM p do
          questionLabeled "findWater.ready"
          causedBy attrs
          targets heroes encounterReadies
      FindWater <$> runMessage msg attrs
    _ -> FindWater <$> runMessage msg attrs
