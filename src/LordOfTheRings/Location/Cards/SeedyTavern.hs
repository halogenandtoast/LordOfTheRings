module LordOfTheRings.Location.Cards.SeedyTavern (seedyTavern) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SeedyTavern = SeedyTavern LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

seedyTavern :: LocationCard SeedyTavern
seedyTavern = location SeedyTavern Cards.seedyTavern

-- | Immune to player card effects.
instance HasModifiersFor SeedyTavern where
  getModifiersFor (SeedyTavern a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When Seedy Tavern is explored, reveal each facedown card under it.
instance HasAbilities SeedyTavern where
  getAbilities (SeedyTavern a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

{- | Travel: Look at the top 5 cards of the encounter deck. Put one of those cards facedown
under Seedy Tavern and shuffle the rest back into the encounter deck. The first player
chooses.
The cards still under it as it leaves play another way are discarded.
-}
instance RunMessage SeedyTavern where
  runMessage msg l@(SeedyTavern a) = case msg of
    TravelTo lid | lid == a.id -> do
      top <- getTopOfEncounterDeckN 5
      lead <- getLead
      unless (null top) $ chooseOneM lead do
        questionLabeled "seedyTavern.placeUnderneath"
        causedBy a
        showing (map EncounterCard top)
        for_ top \c -> targeting (CardIdTarget c.id) do
          removeFromEncounterDeck c
          placeUnderneath a [EncounterCard c]
          shuffleEncounterDeck
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ [c | EncounterCard c <- a.underneath] (push . RevealCard)
      pure $ SeedyTavern (a {locationUnderneath = []})
    Discard _ (isTarget a -> True) -> release
    AddToVictoryDisplay (isTarget a -> True) -> release
    RemoveFromGame (isTarget a -> True) -> release
    ShuffleIntoEncounterDeck (isTarget a -> True) -> release
    ReturnToTopOfEncounterDeck (isTarget a -> True) -> release
    ReturnToBottomOfEncounterDeck (isTarget a -> True) -> release
    _ -> SeedyTavern <$> runMessage msg a
   where
    release = do
      let held = [c | EncounterCard c <- a.underneath]
      unless (null held) $ push $ AddToEncounterDiscard held
      SeedyTavern <$> runMessage msg (a {locationUnderneath = []})
