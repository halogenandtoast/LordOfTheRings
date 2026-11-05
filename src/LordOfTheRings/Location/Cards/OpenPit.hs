module LordOfTheRings.Location.Cards.OpenPit (openPit) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype OpenPit = OpenPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

openPit :: LocationCard OpenPit
openPit = location OpenPit Cards.openPit

{- | While Open Pit is in the staging area, it gains: "Forced: After a player draws any
number of cards from a player card effect, he raises his threat by 2."
Wizardry: Each player raises his threat by 2 (see 'wizardry').
-}
instance HasAbilities OpenPit where
  getAbilities (OpenPit a) =
    [mkAbility a 1 $ forced $ DrewCardsFromPlayerCardWindow After | a.placement == StagingArea]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage OpenPit where
  runMessage msg l@(OpenPit a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (nub [pid | Window _ (CardsDrawnBy pid source) <- ws, isPlayerCardSource source]) \pid ->
        encounterRaisesThreat pid 2
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> OpenPit <$> runMessage msg a
