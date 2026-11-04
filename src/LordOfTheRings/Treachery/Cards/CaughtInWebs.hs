module LordOfTheRings.Treachery.Cards.CaughtInWebs (caughtInWebs) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaughtInWebs = CaughtInWebs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caughtInWebs :: TreacheryCard CaughtInWebs
caughtInWebs = treachery CaughtInWebs Cards.caughtInWebs

{- | When Revealed: Choose a hero to be removed from the quest and make that hero
unconscious. If no hero was removed from the quest by this effect, this card gains surge.
Only a hero committed to the quest can be removed from it: the first player chooses among
those.
-}
instance RunMessage CaughtInWebs where
  runMessage msg t@(CaughtInWebs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- select CommittedHero
      if null committed
        then revealEncounterCard
        else do
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled "caughtInWebs.hero"
            causedBy attrs
            targets committed \h -> do
              removeFromQuest h
              makeUnconscious h
      pure t
    _ -> CaughtInWebs <$> runMessage msg attrs
