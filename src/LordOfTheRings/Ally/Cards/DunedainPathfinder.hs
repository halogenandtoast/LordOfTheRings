module LordOfTheRings.Ally.Cards.DunedainPathfinder (dunedainPathfinder) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType))

newtype DunedainPathfinder = DunedainPathfinder AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainPathfinder :: AllyCard DunedainPathfinder
dunedainPathfinder = ally DunedainPathfinder Cards.dunedainPathfinder

-- | Forced: After this ally enters play, search the top 5 cards of the encounter deck for a
-- non-unique location and add it to the staging area. If no location enters play by this
-- effect, discard Dúnedain Pathfinder. Shuffle the encounter deck.
instance HasAbilities DunedainPathfinder where
  getAbilities (DunedainPathfinder a) = [mkAbility a 1 $ forced $ EntersPlayWindow After (toTarget a)]

instance RunMessage DunedainPathfinder where
  runMessage msg s@(DunedainPathfinder a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- getTopOfEncounterDeckN 5
      let locations = filter (cardMatches $ CardWithType LocationType <> NotCard UniqueCard) top
      if null locations
        then discard a
        else chooseOrRunOneM pid do
          questionLabeled "chooseLocation"
          causedBy a
          showing (map EncounterCard top)
          targets locations \c -> do
            removeFromEncounterDeck c
            addToStaging c
      shuffleEncounterDeck
      pure s
    _ -> DunedainPathfinder <$> runMessage msg a
