module LordOfTheRings.Treachery.Cards.PoisonedVapour (poisonedVapour) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PoisonedVapour = PoisonedVapour TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
poisonedVapour :: TreacheryCard PoisonedVapour
poisonedVapour = treachery PoisonedVapour Cards.poisonedVapour

{- | When Revealed: Exhaust each damaged character. Until the end of the combat phase, treat
each damaged character's text box as if it were blank (except for Traits). The characters
damaged as it resolves are the ones blanked, as a lasting effect reaches the cards it
finds as it is made.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage PoisonedVapour where
  runMessage msg t@(PoisonedVapour attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged \c -> do
        exhaust c
        createEffect attrs c [Blank] (UntilEndOfPhaseOf CombatPhase)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment (CardIdTarget cid) pid
      pure t
    _ -> PoisonedVapour <$> runMessage msg attrs
