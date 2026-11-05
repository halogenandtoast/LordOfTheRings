module LordOfTheRings.Enemy.Cards.GundabadHunter (gundabadHunter) where

import LordOfTheRings.Card (cdUnique, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GundabadHunter = GundabadHunter EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, shuffled in by The Ghost of Framsburg's campaign
-- card.
gundabadHunter :: EnemyCard GundabadHunter
gundabadHunter = enemy GundabadHunter Cards.gundabadHunter

{- | When Revealed: Choose a player that is engaged with a unique enemy, if able. Gundabad
Hunter makes an immediate attack against that player. (With none so engaged, any player
is chosen.) The first player chooses.
Shadow: If attacking enemy is unique, it makes an additional attack after this one.
-}
instance RunMessage GundabadHunter where
  runMessage msg e@(GundabadHunter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      engaged <- select (PlayerEngagedWith UniqueEnemy)
      players <- if null engaged then getRemainingPlayers else pure engaged
      chooseOrRunOneM lead do
        questionLabeled "choosePlayer"
        causedBy attrs
        targets players \p -> immediateAttack p attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      e' <- getEnemy eid
      when (cdUnique (toCardDef e')) $ additionalAttack pid eid
      pure e
    _ -> GundabadHunter <$> runMessage msg attrs
