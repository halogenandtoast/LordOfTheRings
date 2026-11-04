module LordOfTheRings.Location.Cards.OvergrownPath (overgrownPath) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers

newtype OvergrownPath = OvergrownPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An objective-location: Guarded (location). Searching for a Way Out makes it immune to
-- player card effects.
overgrownPath :: LocationCard OvergrownPath
overgrownPath = location OvergrownPath Cards.overgrownPath

instance HasAbilities OvergrownPath where
  getAbilities (OvergrownPath a) = objectiveLocationAbilities a

{- | Travel: Shuffle the encounter discard pile into the encounter deck. Each player discards the top card of the
encounter deck and assigns X damage among characters he controls, where X is that card's
threat.
Forced: After the players travel here, advance to the next stage A, if able.
-}
instance RunMessage OvergrownPath where
  runMessage msg l@(OvergrownPath a) = case msg of
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ cards \c -> dealDamageDivided a pid (fromMaybe 0 $ cdThreat $ toCardDef c)
      pure l
    _ -> do
      runObjectiveLocation SideA msg a
      OvergrownPath <$> runMessage msg a
