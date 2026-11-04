module LordOfTheRings.Enemy.Cards.Dagnir (dagnir) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (sideQuestX)

newtype Dagnir = Dagnir EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dagnir :: EnemyCard Dagnir
dagnir = enemy Dagnir Cards.dagnir

{- | Immune to player card effects. Cannot leave the staging area. While at least 3 side
quests are in the victory display, Dagnir is considered to be engaged with each player:
attacked there, and attacking each player in the combat phase, as Durin's Bane.
-}
instance HasModifiersFor Dagnir where
  getModifiersFor (Dagnir a) = do
    sideQuests <- getSideQuestsInVictoryDisplay
    modifySelf a
      $ [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]
      <> [EngagedWithEachPlayer AnyPlayer | sideQuests >= 3]

{- | X is 1 more than the number of side quests in the victory display. Dagnir cannot take
more than X damage from non-quest effects each round: what she has taken from them this
round is kept as a round effect, and X is read as the damage is placed, so a side quest
defeated later in the round lets her take more. A quest card's damage, as Face the
Dragon's, is not limited and does not count.
-}
instance RunMessage Dagnir where
  runMessage msg (Dagnir attrs) = case msg of
    PlaceTokens source (isTarget attrs -> True) Damage n | n > 0 ->
      if isQuestSource source
        then pure $ Dagnir $ attrs & tokensL %~ addTokens Damage n
        else do
          x <- sideQuestX
          Dagnir <$> placeDamageWithinRoundLimit x n attrs
    _ -> Dagnir <$> runMessage msg attrs
