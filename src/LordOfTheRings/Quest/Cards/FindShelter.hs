module LordOfTheRings.Quest.Cards.FindShelter (findShelter) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FindShelter = FindShelter QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge. Time 4, as its card def says.
findShelter :: QuestCard FindShelter
findShelter = quest FindShelter Cards.findShelter

-- | Forced: After the last time counter is removed from this quest card, each player deals
-- 4 damage to a hero he controls. Then, place 4 time counters on this quest card.
instance HasAbilities FindShelter where
  getAbilities (FindShelter a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage FindShelter where
  runMessage msg q@(FindShelter attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> dealDamageToOneHero attrs p 4
      placeTimeCounters attrs attrs 4
      pure q
    _ -> FindShelter <$> runMessage msg attrs
