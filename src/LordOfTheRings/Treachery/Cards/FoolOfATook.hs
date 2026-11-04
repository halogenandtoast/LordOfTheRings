module LordOfTheRings.Treachery.Cards.FoolOfATook (foolOfATook) where

import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (balrogAttacks, getBalrog, getDoom, removeDoomDamage)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FoolOfATook = FoolOfATook TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

foolOfATook :: TreacheryCard FoolOfATook
foolOfATook = treachery FoolOfATook Cards.foolOfATook

{- | Doomed 1. When Revealed: Remove 1 damage token from Doom, Doom, Doom, or The Balrog
makes an attack, if able. The first player decides, between those that can be done (FAQ
1.44).
Shadow: If this attack destroys a character, reveal 1 card from the encounter deck.
-}
instance RunMessage FoolOfATook where
  runMessage msg t@(FoolOfATook attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      damage <- maybe (pure 0) (getDamage . ObjectiveTarget) =<< getDoom
      balrog <- isJust <$> getBalrog
      chooseOrRunOneM lead do
        questionLabeled "foolOfATook.choose"
        causedBy attrs
        when (damage > 0) $ labeled "foolOfATook.removeDamage" $ removeDoomDamage attrs
        when balrog $ labeled "foolOfATook.balrogAttacks" balrogAttacks
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      revealEncounterCard
      pure t
    _ -> FoolOfATook <$> runMessage msg attrs
