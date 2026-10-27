module LordOfTheRings.Treachery.Cards.IllPurpose (illPurpose) where

import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Trait (Trait (Tentacle))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IllPurpose = IllPurpose TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

illPurpose :: TreacheryCard IllPurpose
illPurpose = treachery IllPurpose Cards.illPurpose

-- | When Revealed: All enemies in the staging area engage the player with the highest
-- threat. Then, each player raises his threat by the total threat of all cards in the
-- staging area.
-- Shadow: attacking enemy gets +1 attack. (+3 attack instead if it is a Tentacle.)
instance RunMessage IllPurpose where
  runMessage msg t@(IllPurpose attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      threats <- for players \p -> (p,) . (.threat) <$> getPlayer p
      let highest = foldr (max . snd) 0 threats
      enemies <- select EnemyInStagingArea
      lead <- getLead
      -- with a tie, the first player chooses which of the tied players
      chooseOrRunOneM lead do
        questionLabeled "illPurpose.choosePlayer"
        causedBy attrs
        targets [p | (p, n) <- threats, n == highest] \p -> for_ enemies (engageEnemy p)
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      threat <- getStagingThreat
      eachPlayer \p -> encounterRaisesThreat p threat
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      tentacle <- selectAny (EnemyWithId eid <> EnemyWithTrait Tentacle)
      enemyGetsAttack attrs eid (if tentacle then 3 else 1)
      pure t
    _ -> IllPurpose <$> runMessage msg attrs
