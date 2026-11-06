module LordOfTheRings.Quest.Cards.TerrorFromTheMountain (terrorFromTheMountain) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies, lakeTownObjectives)

newtype TerrorFromTheMountain = TerrorFromTheMountain QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

terrorFromTheMountain :: QuestCard TerrorFromTheMountain
terrorFromTheMountain = quest TerrorFromTheMountain Cards.terrorFromTheMountain

instance HasModifiersFor TerrorFromTheMountain where
  getModifiersFor (TerrorFromTheMountain a) = when a.flipped do
    enemies <- smaugEnemies
    for_ enemies \eid -> modified a eid [MaxDamage 6]

instance RunMessage TerrorFromTheMountain where
  runMessage msg q@(TerrorFromTheMountain a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      push $ ScenarioSpecific "lakeTown.setup" Null
      turnOver a
      pure q
    Flip (isTarget a -> True) | not a.flipped -> do
      askToResolveWhenRevealed a
      TerrorFromTheMountain <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      players <- length <$> getPlayerOrder
      towns <- lakeTownObjectives
      for_ towns \oid -> dealDamage a oid players
      pure q
    _ -> TerrorFromTheMountain <$> runMessage msg a
