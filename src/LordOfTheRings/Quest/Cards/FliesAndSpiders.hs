module LordOfTheRings.Quest.Cards.FliesAndSpiders (fliesAndSpiders) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FliesAndSpiders = FliesAndSpiders QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fliesAndSpiders :: QuestCard FliesAndSpiders
fliesAndSpiders = quest FliesAndSpiders Cards.fliesAndSpiders

instance RunMessage FliesAndSpiders where
  runMessage msg q@(FliesAndSpiders attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) -> do
      lead <- getLead
      send $ FlipCard attrs.cardId
      searchEncounterDeckToStaging lead (cardIs Enemies.forestSpider)
      searchEncounterDeckToStaging lead (cardIs Locations.oldForestRoad)
      pure $ FliesAndSpiders $ attrs {questFlipped = True}
    _ -> FliesAndSpiders <$> runMessage msg attrs
