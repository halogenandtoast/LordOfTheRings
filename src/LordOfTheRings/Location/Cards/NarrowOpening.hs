module LordOfTheRings.Location.Cards.NarrowOpening (narrowOpening) where

import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype NarrowOpening = NarrowOpening LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowOpening :: LocationCard NarrowOpening
narrowOpening = location NarrowOpening Cards.narrowOpening

-- | While Narrow Opening is the active location, each player cannot play more than 1 card
-- each round: any card he played this round counts, one before it became active too.
instance HasModifiersFor NarrowOpening where
  getModifiersFor (NarrowOpening a) = when (a.placement == ActiveLocation) do
    played <- map (\(p, _, _) -> p) <$> cardsPlayedThisRound
    for_ (nub played) \p -> modified a (PlayerTarget p) [CannotPlay AnyCard]

-- | Forced: When Narrow Opening leaves play, each player discards 1 random card from his
-- hand. Read in the window as it leaves, which nothing comes before.
instance HasAbilities NarrowOpening where
  getAbilities (NarrowOpening a) = [mkAbility a 1 $ forced $ LocationLeftPlayWindow After a.id]

instance RunMessage NarrowOpening where
  runMessage msg l@(NarrowOpening a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer (`discardRandomFromHand` 1)
      pure l
    _ -> NarrowOpening <$> runMessage msg a
