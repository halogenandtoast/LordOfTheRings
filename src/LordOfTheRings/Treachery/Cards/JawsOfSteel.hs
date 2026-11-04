module LordOfTheRings.Treachery.Cards.JawsOfSteel (jawsOfSteel) where

import LordOfTheRings.Scenario.Scenarios.TheBlackGateOpens.Helpers (blackGateResources)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype JawsOfSteel = JawsOfSteel TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jawsOfSteel :: TreacheryCard JawsOfSteel
jawsOfSteel = treachery JawsOfSteel Cards.jawsOfSteel

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the number of resources on The Black Gate.
-- Shadow: Attacking enemy gets +1 attack for each resource on The Black Gate.
instance RunMessage JawsOfSteel where
  runMessage msg t@(JawsOfSteel attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- blackGateResources
      when (x > 0) $ eachPlayer \pid -> dealDamageDivided attrs pid x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- blackGateResources
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure t
    _ -> JawsOfSteel <$> runMessage msg attrs
