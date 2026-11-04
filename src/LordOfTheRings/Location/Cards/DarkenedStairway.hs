module LordOfTheRings.Location.Cards.DarkenedStairway (darkenedStairway) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DarkenedStairway = DarkenedStairway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkenedStairway :: LocationCard DarkenedStairway
darkenedStairway = location DarkenedStairway Cards.darkenedStairway

-- | Forced: After Darkened Stairway is explored, discard the top card of the encounter
-- deck. If the discarded card is a location, add it to the staging area.
instance HasAbilities DarkenedStairway where
  getAbilities (DarkenedStairway a) = [mkAbility a 1 $ forced $ ExploredWindow After a.id]

instance RunMessage DarkenedStairway where
  runMessage msg l@(DarkenedStairway a) = case msg of
    -- gone from play, it hears only its ability: the top card is read as it is discarded
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      discardTopOfEncounterDeck a 1
      for_ (take 1 deck) \c -> when (cardMatches (CardWithType LocationType) c) $ addToStagingFromDiscard c
      pure l
    _ -> DarkenedStairway <$> runMessage msg a
