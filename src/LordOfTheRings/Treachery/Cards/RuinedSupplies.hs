module LordOfTheRings.Treachery.Cards.RuinedSupplies (ruinedSupplies) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RuinedSupplies = RuinedSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedSupplies :: TreacheryCard RuinedSupplies
ruinedSupplies = treachery RuinedSupplies Cards.ruinedSupplies

-- | Doomed 1.
-- When Revealed: Each player must choose: either raise your threat by 1 for each ally you
-- control, or deal 1 damage to each ally you control.
-- Shadow: Raise your threat by 1 for each enemy engaged with you.
instance RunMessage RuinedSupplies where
  runMessage msg t@(RuinedSupplies attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOneM p do
          questionLabeled "ruinedSupplies.choose"
          causedBy attrs
          labeledWith "ruinedSupplies.raiseThreat" [intVar "n" (length allies)]
            $ encounterRaisesThreat p (length allies)
          labeled "ruinedSupplies.damage" $ for_ allies \a -> dealDamage attrs a 1
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      when (engaged > 0) $ encounterRaisesThreat pid engaged
      pure t
    _ -> RuinedSupplies <$> runMessage msg attrs
