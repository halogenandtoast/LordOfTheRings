module LordOfTheRings.Location.Cards.RedhornFoothills (redhornFoothills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype RedhornFoothills = RedhornFoothills LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

redhornFoothills :: LocationCard RedhornFoothills
redhornFoothills = location RedhornFoothills Cards.redhornFoothills

{- | While Redhorn Foothills is the active location, it gains: "Forced: After an enemy is
added to the staging area, it gets -5 engagement cost until the end of the round."
Forced: When Redhorn Foothills is explored, each player must discard X cards from his hand
at random. X is the number of damage here.
-}
instance HasAbilities RedhornFoothills where
  getAbilities (RedhornFoothills a) =
    [mkAbility a 1 $ forced $ EnemyEntersStagingAreaWindow After | a.placement == ActiveLocation]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage RedhornFoothills where
  runMessage msg l@(RedhornFoothills a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- ws] \eid ->
        roundModifier a eid (EngagementCostModifier (-5))
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      when (a.damage > 0) $ eachPlayer \pid -> discardRandomFromHand pid a.damage
      pure l
    _ -> RedhornFoothills <$> runMessage msg a
