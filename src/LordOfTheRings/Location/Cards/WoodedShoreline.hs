module LordOfTheRings.Location.Cards.WoodedShoreline (woodedShoreline) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype WoodedShoreline = WoodedShoreline LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodedShoreline :: LocationCard WoodedShoreline
woodedShoreline = location WoodedShoreline Cards.woodedShoreline

-- | While Wooded Shoreline is in the staging area, it gains archery 2.
instance HasModifiersFor WoodedShoreline where
  getModifiersFor (WoodedShoreline a) = when (a.placement == StagingArea) $ modifySelf a [GainKeyword (Archery 2)]

-- | Travel: Search the encounter deck and discard pile for an enemy and add it to the
-- staging area to travel here. Shuffle the encounter deck.
instance RunMessage WoodedShoreline where
  runMessage msg (WoodedShoreline a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getStageLead
      searchEncounterDeckAndDiscard lead a (CardWithType EnemyType)
      WoodedShoreline <$> runMessage msg a
    _ -> WoodedShoreline <$> runMessage msg a
