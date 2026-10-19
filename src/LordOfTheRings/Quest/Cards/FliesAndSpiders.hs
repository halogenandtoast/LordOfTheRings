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
      lead <- getLead
      push $ questionLabel "Perform setup: flip Flies and Spiders" lead [targetLabel attrs [Flip $ toTarget attrs]]
      pure q
    Flip (isTarget attrs -> True) -> do
      lead <- getLead
      send $ FlipCard attrs.cardId
      pushAll
        [ Search lead EncounterDeck (cardIs Enemies.forestSpider) ToStaging Shuffle
        , Search lead EncounterDeck (cardIs Locations.oldForestRoad) ToStaging Shuffle
        ]
      pure $ FliesAndSpiders $ attrs {questFlipped = True}
    _ -> FliesAndSpiders <$> runMessage msg attrs
