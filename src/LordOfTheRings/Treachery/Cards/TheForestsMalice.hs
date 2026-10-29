module LordOfTheRings.Treachery.Cards.TheForestsMalice (theForestsMalice) where

import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (searchForHuornEngaged)
import LordOfTheRings.Trait (Trait (Huorn))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheForestsMalice = TheForestsMalice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theForestsMalice :: TreacheryCard TheForestsMalice
theForestsMalice = treachery TheForestsMalice Cards.theForestsMalice

-- | This effect cannot be canceled.
instance HasModifiersFor TheForestsMalice where
  getModifiersFor (TheForestsMalice a) = modified a a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Each Huorn enemy engaged with a player makes an immediate attack. If no
-- attack was made as a result of this effect, each player must search the encounter deck
-- and discard pile for a Huorn enemy and put it into play engaged with him.
-- ponytail: an engaged Huorn counts as attacking even if a card stops its attack later
instance RunMessage TheForestsMalice where
  runMessage msg t@(TheForestsMalice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      engaged <- selectAny (EnemyWithTrait Huorn <> EnemyEngagedWith AnyPlayer)
      eachPlayer \pid ->
        if engaged
          then traverse_ (immediateAttack pid) =<< select (EnemyWithTrait Huorn <> EnemyEngagedWith (PlayerWithId pid))
          else searchForHuornEngaged pid
      pure t
    _ -> TheForestsMalice <$> runMessage msg attrs
