module LordOfTheRings.Location.Cards.HelmsGate (helmsGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (firstPlayerReveals)

newtype HelmsGate = HelmsGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

helmsGate :: LocationCard HelmsGate
helmsGate = location HelmsGate Cards.helmsGate

{- | While Helm's Gate is the active location, if any player is not engaged with an enemy,
the first player reveals an encounter card at the end of each staging step.

Forced: When Helm's Gate is explored, each enemy gets -20 engagement cost and +2 attack
until the end of the round: each enemy in play as it is explored.
-}
instance HasAbilities HelmsGate where
  getAbilities (HelmsGate a) =
    [ restricted a 1 (PlayerExists $ NotPlayer $ PlayerEngagedWith AnyEnemy) $ forced StagingStepEndsWindow
    | a.placement == ActiveLocation
    ]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage HelmsGate where
  runMessage msg l@(HelmsGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      firstPlayerReveals
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      enemies <- select AnyEnemy
      for_ enemies \e -> roundModifiers a e [EngagementCostModifier (-20), AttackModifier 2]
      pure l
    _ -> HelmsGate <$> runMessage msg a
