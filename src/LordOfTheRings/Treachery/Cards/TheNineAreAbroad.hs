module LordOfTheRings.Treachery.Cards.TheNineAreAbroad (theNineAreAbroad) where

import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheNineAreAbroad = TheNineAreAbroad TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
theNineAreAbroad :: TreacheryCard TheNineAreAbroad
theNineAreAbroad = treachery TheNineAreAbroad Cards.theNineAreAbroad

-- | When Revealed: Treat the text box of each character in play as blank (except for
-- Traits) until the end of the round. If there is a Nazgûl enemy in the staging area, The
-- Nine are Abroad gains surge. A blank text box keeps its card's traits.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage TheNineAreAbroad where
  runMessage msg t@(TheNineAreAbroad attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifier attrs c Blank
      whenM (selectAny $ EnemyWithTrait Nazgul <> EnemyInStagingArea) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> TheNineAreAbroad <$> runMessage msg attrs
