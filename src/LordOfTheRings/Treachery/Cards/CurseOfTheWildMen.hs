module LordOfTheRings.Treachery.Cards.CurseOfTheWildMen (curseOfTheWildMen) where

import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (controlledKeys)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CurseOfTheWildMen = CurseOfTheWildMen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

curseOfTheWildMen :: TreacheryCard CurseOfTheWildMen
curseOfTheWildMen = treachery CurseOfTheWildMen Cards.curseOfTheWildMen

-- | When Revealed: Deal 1 damage to each non-unique character in play. If the players
-- control 3 Key objectives, Curse of the Wild Men gains Surge.
-- Shadow: Attacking enemy gets +1 attack for each Key objective the players control.
-- (Every hero is unique, so only allies can be non-unique characters.)
instance RunMessage CurseOfTheWildMen where
  runMessage msg t@(CurseOfTheWildMen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- select (not_ UniqueAlly)
      for_ allies \ally -> dealDamage attrs ally 1
      held <- length <$> controlledKeys
      when (held >= 3) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      held <- length <$> controlledKeys
      when (held > 0) $ enemyGetsAttack attrs eid held
      pure t
    _ -> CurseOfTheWildMen <$> runMessage msg attrs
