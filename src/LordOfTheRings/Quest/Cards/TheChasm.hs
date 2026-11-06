module LordOfTheRings.Quest.Cards.TheChasm (theChasm) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheChasm = TheChasm QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theChasm :: QuestCard TheChasm
theChasm = quest TheChasm Cards.theChasm

instance HasModifiersFor TheChasm where
  getModifiersFor (TheChasm a) = when a.flipped do
    modified a GameTarget [AdditionalStagingReveals 1]
    pillars <-
      select (LocationInStagingArea <> LocationWithCardCode (toCardCode Locations.crackedPillar))
    for_ pillars \lid -> modified a lid [ProgressLimit 0]

instance HasAbilities TheChasm where
  getAbilities (TheChasm a) =
    [mkAbility a 1 $ forced $ ExploredMatchingWindow After (cardIs Locations.crackedPillar) | a.flipped]

instance RunMessage TheChasm where
  runMessage msg q@(TheChasm attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheChasm <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      chiefs <- select (EnemyWithCardCode $ toCardCode Enemies.goblinChieftain)
      traverse_ setAsideEnemy chiefs
      addSetAsideToStaging (cardIs Locations.crackedPillar)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> completeQuest attrs.id >> pure q
    _ -> TheChasm <$> runMessage msg attrs
