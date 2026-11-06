module LordOfTheRings.Quest.Cards.AfterTheRaid (afterTheRaid) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AfterTheRaid = AfterTheRaid QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

afterTheRaid :: QuestCard AfterTheRaid
afterTheRaid = quest AfterTheRaid Cards.afterTheRaid

instance RunMessage AfterTheRaid where
  runMessage msg q@(AfterTheRaid attrs) = case msg of
    DoStep 7 Setup -> askToPerformSetup attrs >> pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ setAside $ find ((== toCardCode Enemies.goblinTroopDarkOfMirkwood) . toCardCode) deck
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Locations.theEavesOfMirkwoodDarkOfMirkwood)
      turnOver attrs
      pure q
    _ -> AfterTheRaid <$> runMessage msg attrs
