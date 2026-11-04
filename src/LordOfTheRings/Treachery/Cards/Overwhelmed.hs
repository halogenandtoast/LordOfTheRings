module LordOfTheRings.Treachery.Cards.Overwhelmed (overwhelmed) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Overwhelmed = Overwhelmed TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overwhelmed :: TreacheryCard Overwhelmed
overwhelmed = treachery Overwhelmed Cards.overwhelmed

-- | Doomed 1.
-- When Revealed: Until the end of the round, enemies engaged with the first player cannot
-- take damage: those engaged with him as it resolves.
-- Shadow: Either raise your threat by 2, or attacking enemy cannot take damage this round.
instance RunMessage Overwhelmed where
  runMessage msg t@(Overwhelmed attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      engaged <- select (EnemyEngagedWith $ PlayerWithId lead)
      for_ engaged \e -> roundModifier attrs e CannotTakeDamage
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "overwhelmed.choose"
        causedBy attrs
        labeled "overwhelmed.raiseThreat" $ encounterRaisesThreat pid 2
        labeled "overwhelmed.noDamage" $ roundModifier attrs eid CannotTakeDamage
      pure t
    _ -> Overwhelmed <$> runMessage msg attrs
