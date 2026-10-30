module LordOfTheRings.Treachery.Cards.UnnaturalFog (unnaturalFog) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnnaturalFog = UnnaturalFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unnaturalFog :: TreacheryCard UnnaturalFog
unnaturalFog = treachery UnnaturalFog Cards.unnaturalFog

-- | Doomed 2.
-- When Revealed: Each player must either discard 1 card from the top of his deck for each
-- questing character he controls, or remove each character he controls from the quest.
-- A player with no questing character has nothing to choose.
instance RunMessage UnnaturalFog where
  runMessage msg t@(UnnaturalFog attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> CommittedHero) (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
        unless (null questing) $ chooseOneM pid do
          questionLabeled "unnaturalFog.choose"
          causedBy attrs
          labeledWith "unnaturalFog.discard" [intVar "count" (length questing)] $ discardTopOfDeck pid (length questing)
          labeled "unnaturalFog.removeFromQuest" $ traverse_ removeFromQuest questing
      pure t
    _ -> UnnaturalFog <$> runMessage msg attrs
