module LordOfTheRings.Location.Cards.HideousDeeps (hideousDeeps) where

import LordOfTheRings.Card (Card (EncounterCard, PlayerCard))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype HideousDeeps = HideousDeeps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hideousDeeps :: LocationCard HideousDeeps
hideousDeeps = location HideousDeeps Cards.hideousDeeps

-- | Forced: When Hideous Deeps is explored as the active location, shuffle each facedown
-- card under it into its owner's deck.
instance HasAbilities HideousDeeps where
  getAbilities (HideousDeeps a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id | a.placement == ActiveLocation]

-- | When Revealed: Each player places the top 3 cards of his deck facedown under Hideous
-- Deeps. Explored any other way, or leaving play otherwise, the cards stay out of play.
-- An encounter card that was in a player's deck goes back to the encounter deck, its
-- owner's.
instance RunMessage HideousDeeps where
  runMessage msg l@(HideousDeeps attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        deck <- (.deck) <$> getPlayer p
        placeUnderneath attrs (take 3 deck)
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ attrs.underneath \case
        PlayerCard pc -> for_ pc.owner \owner -> shuffleIntoDeck owner (PlayerCard pc)
        EncounterCard ec -> do
          setAside ec
          shuffleSetAsideIntoEncounterDeck [ec]
      pure $ HideousDeeps $ attrs & underneathL .~ []
    _ -> HideousDeeps <$> runMessage msg attrs
