module LordOfTheRings.Treachery.Cards.BlackMare (blackMare) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (getThaurdir)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackMare = BlackMare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackMare :: TreacheryCard BlackMare
blackMare = treachery BlackMare Cards.blackMare

-- | Counts as a Mount attachment with the text: "Black Mare remains attached even if
-- Thaurdir flips. Thaurdir gets +3 hit points. Forced: After Thaurdir is flipped, raise
-- each player's threat by 1." Thaurdir turns over in place, so it stays on him. It goes
-- on him though he cannot have attachments, as it says.
instance HasModifiersFor BlackMare where
  getModifiersFor (BlackMare a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 3]
    _ -> pure ()

-- | When Revealed: Attach to Thaurdir.
-- Shadow: Attach Black Mare to Thaurdir.
instance RunMessage BlackMare where
  runMessage msg t@(BlackMare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getThaurdir >>= traverse_ (attachTreachery attrs.treacheryId)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      getThaurdir >>= traverse_ (attachShadowTreachery eid cid)
      pure t
    Flip (EnemyTarget eid) | AttachedTo (EnemyTarget host) <- attrs.placement, eid == host -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      BlackMare <$> runMessage msg attrs
    _ -> BlackMare <$> runMessage msg attrs
