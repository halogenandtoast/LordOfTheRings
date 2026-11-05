module LordOfTheRings.Location.Cards.UndergroundArmoury (undergroundArmoury) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype UndergroundArmoury = UndergroundArmoury LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

undergroundArmoury :: LocationCard UndergroundArmoury
undergroundArmoury = location UndergroundArmoury Cards.undergroundArmoury

{- | While Underground Armoury is in the staging area, it gains: "Forced: After an enemy
engages a player, deal it a shadow card."
Wizardry: Each player exhausts a character he controls (see 'wizardry').
-}
instance HasAbilities UndergroundArmoury where
  getAbilities (UndergroundArmoury a) =
    [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.placement == StagingArea]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage UndergroundArmoury where
  runMessage msg l@(UndergroundArmoury a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- ws] dealShadowCard
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> UndergroundArmoury <$> runMessage msg a
