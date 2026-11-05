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
each damaged character's text box as if it were blank (except for Traits). The exhausting
happens once; the blank is one lasting effect, recalculated as the game state changes: a
damaged character healed is no longer blank, one damaged later is, until the end of the
combat phase (FAQ 1.55, which rules on this card). Aragorn's "if Aragorn leaves play, the
players lose the game" cannot be modified by encounter card effects (MEC113 rules): his
card resolves it whatever his modifiers, so the blank leaves it.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage PoisonedVapour where
  runMessage msg t@(PoisonedVapour attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged exhaust
      createEffect attrs GameTarget [EachDamagedCharacter Blank] (UntilEndOfPhaseOf CombatPhase)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment (CardIdTarget cid) pid
      pure t
    _ -> PoisonedVapour <$> runMessage msg attrs
