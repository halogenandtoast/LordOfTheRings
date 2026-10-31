module LordOfTheRings.Treachery.Cards.BlindingBlizzard (blindingBlizzard) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Snow))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlindingBlizzard = BlindingBlizzard TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blindingBlizzard :: TreacheryCard BlindingBlizzard
blindingBlizzard = treachery BlindingBlizzard Cards.blindingBlizzard

-- | While Blinding Blizzard is in play, heroes do not collect resources during the
-- resource phase.
-- Progress that would be placed on the current quest or on Snow locations are instead
-- placed on Blinding Blizzard.
instance HasModifiersFor BlindingBlizzard where
  getModifiersFor (BlindingBlizzard a) = when (a.placement == StagingArea) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotCollectResources]
    quest <- getCurrentQuest
    snow <- map toTarget <$> select (LocationWithTrait Snow)
    for_ (toList quest <> snow) \t -> modified a t [ProgressPlacedOnInstead (toTarget a)]

-- | When Revealed: Add Blinding Blizzard to the staging area. When the 5th progress is
-- placed on Blinding Blizzard, discard it.
instance RunMessage BlindingBlizzard where
  runMessage msg (BlindingBlizzard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> pure $ BlindingBlizzard $ attrs & placementL .~ StagingArea
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      attrs' <- runMessage msg attrs
      when (countTokens Progress (treacheryTokens attrs') >= 5) $ discard attrs
      pure $ BlindingBlizzard attrs'
    _ -> BlindingBlizzard <$> runMessage msg attrs
