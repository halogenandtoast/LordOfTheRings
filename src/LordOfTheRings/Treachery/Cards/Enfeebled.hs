module LordOfTheRings.Treachery.Cards.Enfeebled (enfeebled) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Enfeebled = Enfeebled TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

enfeebled :: TreacheryCard Enfeebled
enfeebled = treachery Enfeebled Cards.enfeebled

-- | When Revealed: Each player exhausts a character he controls. Until the end of the
-- combat phase, exhausted characters cannot ready: a ready character readies to nothing,
-- so no character readies, the refresh phase's coming after it.
-- Shadow: Until the end of the round, defending character cannot ready.
instance RunMessage Enfeebled where
  runMessage msg t@(Enfeebled attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseAndExhaust attrs p 1
      createEffect attrs GameTarget [CannotReady] (UntilEndOfPhaseOf CombatPhase)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d CannotReady
      pure t
    _ -> Enfeebled <$> runMessage msg attrs
