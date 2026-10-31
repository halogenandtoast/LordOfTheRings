module LordOfTheRings.Quest.Cards.ShadowTheWarParty (shadowTheWarParty) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ShadowTheWarParty = ShadowTheWarParty QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
shadowTheWarParty :: QuestCard ShadowTheWarParty
shadowTheWarParty = quest ShadowTheWarParty Cards.shadowTheWarParty

warParty :: EnemyMatcher
warParty = EnemyWithCardCode (toCardCode Enemies.orcWarParty)

-- | Each copy of Orc War Party cannot be engaged, cannot be dealt damage, and gets +2
-- threat. Neither engagement checks nor the players may engage it.
instance HasModifiersFor ShadowTheWarParty where
  getModifiersFor (ShadowTheWarParty a) = do
    players <- getRemainingPlayers
    parties <- select warParty
    for_ parties \e ->
      modified a e $ [CannotTakeDamage, ThreatModifier 2, CannotBeOptionallyEngagedBy AnyPlayer] <> map CannotEngagePlayer players

-- | When Revealed: Return each engaged Orc War Party to the staging area.
instance RunMessage ShadowTheWarParty where
  runMessage msg q@(ShadowTheWarParty attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      engaged <- select (warParty <> EnemyEngagedWith AnyPlayer)
      for_ engaged \e -> placeEnemy e StagingArea
      pure q
    _ -> ShadowTheWarParty <$> runMessage msg attrs
