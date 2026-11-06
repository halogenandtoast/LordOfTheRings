module LordOfTheRings.Quest.Cards.TheDragonsWrath (theDragonsWrath) where

import LordOfTheRings.Card (EncounterCard (..), genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownDamage)
import LordOfTheRings.Scenario.Types ()

newtype TheDragonsWrath = TheDragonsWrath QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theDragonsWrath :: QuestCard TheDragonsWrath
theDragonsWrath = quest TheDragonsWrath Cards.theDragonsWrath

instance HasModifiersFor TheDragonsWrath where
  getModifiersFor (TheDragonsWrath a) = when a.flipped do
    modified a GameTarget [AdditionalStagingReveals 1]

instance HasAbilities TheDragonsWrath where
  getAbilities (TheDragonsWrath a) =
    [anyPlayer $ restricted a 1 (DuringPhase RefreshPhase) $ ActionAbility Free | a.flipped]

instance RunMessage TheDragonsWrath where
  runMessage msg q@(TheDragonsWrath a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      stage2 <- genEncounterCard Cards.fireAndWater
      resetToStage stage2 [MkEncounterCard a.cardId (toCardCode a)]
      pure q
    Destroy _ (EnemyTarget eid) | a.flipped -> do
      smaug <- selectAny (EnemyWithId eid <> EnemyWithTitleContaining "Smaug")
      when smaug do
        damage <- lakeTownDamage
        points <- getsGame ((.victoryPoints) . toAttrs . (.scenario))
        if points > damage then winGame else gameOver Lost
      TheDragonsWrath <$> runMessage msg a
    _ -> TheDragonsWrath <$> runMessage msg a
