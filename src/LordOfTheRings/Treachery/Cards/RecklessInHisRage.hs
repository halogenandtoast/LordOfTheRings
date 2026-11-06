module LordOfTheRings.Treachery.Cards.RecklessInHisRage (recklessInHisRage) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies)

newtype RecklessInHisRage = RecklessInHisRage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

recklessInHisRage :: TreacheryCard RecklessInHisRage
recklessInHisRage = treachery RecklessInHisRage Cards.recklessInHisRage

instance RunMessage RecklessInHisRage where
  runMessage msg t@(RecklessInHisRage a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      enemies <- smaugEnemies
      for_ enemies \eid -> roundModifiers a eid [ThreatModifier 2, AttackModifier 2, DefenseModifier (-2), AlliesCannotBeDeclaredDefenders]
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> t <$ ifThisAttackDestroys eid msg
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> t <$ resolveBurnDamage (CardIdSource cid)
    _ -> RecklessInHisRage <$> runMessage msg a
