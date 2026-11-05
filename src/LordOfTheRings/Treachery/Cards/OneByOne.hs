module LordOfTheRings.Treachery.Cards.OneByOne (oneByOne) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Trait (Trait (Sack))

newtype OneByOne = OneByOne TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oneByOne :: TreacheryCard OneByOne
oneByOne = treachery OneByOne Cards.oneByOne

instance RunMessage OneByOne where
  runMessage msg t@(OneByOne attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      sacked <- selectCharacters (HeroWithAttached Sack) (AllyWithAttached Sack)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "oneByOne.destroy"
        causedBy attrs
        targets sacked \c -> do
          sacks <- attachedWithTrait Sack c
          for_ sacks \s -> phaseModifier attrs s LeavesPlayToTopOfSackDeck
          destroy attrs c
          doStep 1 (Destroy (toSource attrs) c)
      pure t
    DoStep 1 (Destroy (isSource attrs -> True) c) -> do
      characters <- selectCharacters AnyHero AnyAlly
      when (c `notElem` characters) $ sack 1
      pure t
    _ -> OneByOne <$> runMessage msg attrs
