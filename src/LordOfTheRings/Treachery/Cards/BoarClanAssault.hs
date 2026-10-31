module LordOfTheRings.Treachery.Cards.BoarClanAssault (boarClanAssault) where

import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BoarClanAssault = BoarClanAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarClanAssault :: TreacheryCard BoarClanAssault
boarClanAssault = treachery BoarClanAssault Cards.boarClanAssault

-- | When Revealed: Each engaged enemy makes an immediate attack. If no attacks are made
-- this way, Boar Clan Assault gains surge.
-- Shadow: Each enemy engaged with you gets +1 attack until the end of the phase.
instance RunMessage BoarClanAssault where
  runMessage msg t@(BoarClanAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse getEnemy =<< select AnyEnemy
      let engaged = [(p, toId e) | e <- enemies, EngagedWith p <- [e.placement]]
      if null engaged
        then revealEncounterCard
        else for_ engaged \(p, eid) -> immediateAttack p eid
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
      for_ engaged \e -> phaseModifier attrs e (AttackModifier 1)
      pure t
    _ -> BoarClanAssault <$> runMessage msg attrs
