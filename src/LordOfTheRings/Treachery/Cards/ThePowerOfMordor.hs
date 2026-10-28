module LordOfTheRings.Treachery.Cards.ThePowerOfMordor (thePowerOfMordor) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ThePowerOfMordor = ThePowerOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePowerOfMordor :: TreacheryCard ThePowerOfMordor
thePowerOfMordor = treachery ThePowerOfMordor Cards.thePowerOfMordor

-- | Doomed 3.
-- When Revealed: Count the number of encounter cards in the staging area and shuffle them
-- into the encounter deck. Then, reveal an equal number of cards from the encounter deck
-- and add them to the staging area. This effect cannot be canceled.
-- ponytail: enemies and locations only, the staging area's objectives stay
instance RunMessage ThePowerOfMordor where
  runMessage msg t@(ThePowerOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- map toTarget <$> select EnemyInStagingArea
      locations <- map toTarget <$> select LocationInStagingArea
      let staged = enemies <> locations
      traverse_ shuffleIntoEncounterDeck staged
      revealEncounterCards (length staged)
      pure t
    _ -> ThePowerOfMordor <$> runMessage msg attrs
