module LordOfTheRings.Quest.Cards.GuardiansOfTheJungle (guardiansOfTheJungle) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype GuardiansOfTheJungle = GuardiansOfTheJungle QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Its text is Hall of Beorn's: cards.json has only its flavor text.
guardiansOfTheJungle :: QuestCard GuardiansOfTheJungle
guardiansOfTheJungle = quest GuardiansOfTheJungle Cards.guardiansOfTheJungle

-- | Each location gets +1 threat.
instance HasModifiersFor GuardiansOfTheJungle where
  getModifiersFor (GuardiansOfTheJungle a) = do
    locations <- select AnyLocation
    for_ locations \l -> modified a l [ThreatModifier 1]

-- | Forced: When Guardians of the Jungle becomes the current quest, search the encounter
-- deck and discard pile for a Territorial Ape and put it into play engaged with the first
-- player.
instance RunMessage GuardiansOfTheJungle where
  runMessage msg (GuardiansOfTheJungle attrs) = case msg of
    SetCurrentQuest (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToEngage lead attrs (cardIs Enemies.territorialApe)
      GuardiansOfTheJungle <$> runMessage msg attrs
    _ -> GuardiansOfTheJungle <$> runMessage msg attrs
