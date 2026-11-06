module LordOfTheRings.Location.Cards.CrumblingStairsDarkOfMirkwood (crumblingStairsDarkOfMirkwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CrumblingStairsDarkOfMirkwood = CrumblingStairsDarkOfMirkwood LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumblingStairsDarkOfMirkwood :: LocationCard CrumblingStairsDarkOfMirkwood
crumblingStairsDarkOfMirkwood = location CrumblingStairsDarkOfMirkwood Cards.crumblingStairsDarkOfMirkwood

instance HasModifiersFor CrumblingStairsDarkOfMirkwood where
  getModifiersFor (CrumblingStairsDarkOfMirkwood a) = when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]

instance HasAbilities CrumblingStairsDarkOfMirkwood where
  getAbilities (CrumblingStairsDarkOfMirkwood a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage CrumblingStairsDarkOfMirkwood where
  runMessage msg l@(CrumblingStairsDarkOfMirkwood a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardTopOfEncounterDeck a 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType LocationType) cards) \c -> do
        removeFromEncounterDiscard c
        putIntoPlayAsActiveLocation c
      pure l
    _ -> CrumblingStairsDarkOfMirkwood <$> runMessage msg a
