module LordOfTheRings.Treachery.Cards.UrdugsCommand (urdugsCommand) where

import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (getUrdug, placeAssaultProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UrdugsCommand = UrdugsCommand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugsCommand :: TreacheryCard UrdugsCommand
urdugsCommand = treachery UrdugsCommand Cards.urdugsCommand

-- | When Revealed: Urdug makes an immediate attack against the first player. If this attack
-- destroys a character, place 1 progress on The Goblins' Assault.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place 1
-- progress on The Goblins' Assault.
instance RunMessage UrdugsCommand where
  runMessage msg t@(UrdugsCommand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      getUrdug >>= traverse_ \u -> do
        immediateAttack lead u
        ifImmediateAttackDestroys u msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      placeAssaultProgress attrs 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeAssaultProgress attrs 1
      pure t
    _ -> UrdugsCommand <$> runMessage msg attrs
