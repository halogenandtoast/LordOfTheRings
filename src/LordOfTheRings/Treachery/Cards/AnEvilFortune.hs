module LordOfTheRings.Treachery.Cards.AnEvilFortune (anEvilFortune) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AnEvilFortune = AnEvilFortune TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

anEvilFortune :: TreacheryCard AnEvilFortune
anEvilFortune = treachery AnEvilFortune Cards.anEvilFortune

-- | When Revealed: Add this card to the victory display.
-- Shadow: If this attack destroys a character, add this card to the victory display.
instance RunMessage AnEvilFortune where
  runMessage msg t@(AnEvilFortune attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addToVictoryDisplay attrs
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    -- it leaves the enemy for the discard pile, and from there the victory display
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      discardShadowCard eid (MkEncounterCard cid (toCardCode attrs))
      addToVictoryDisplay (CardIdTarget cid)
      pure t
    _ -> AnEvilFortune <$> runMessage msg attrs
