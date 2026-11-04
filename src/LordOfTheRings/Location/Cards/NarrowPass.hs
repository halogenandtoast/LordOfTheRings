module LordOfTheRings.Location.Cards.NarrowPass (narrowPass) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (removeWainridersDamage)

newtype NarrowPass = NarrowPass LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowPass :: LocationCard NarrowPass
narrowPass = location NarrowPass Cards.narrowPass

-- | While Narrow Pass is the active location, it gains: "Response: When damage would be
-- placed on Wainrider Champion, remove an equal amount of damage from The Wainriders'
-- stage instead."
instance HasAbilities NarrowPass where
  getAbilities (NarrowPass a) =
    [ mkAbility a 1 $ response $ MatchingEnemyDealtDamageWindow When (EnemyWithCardCode $ toCardCode Enemies.wainriderChampion)
    | a.placement == ActiveLocation
    ]

instance RunMessage NarrowPass where
  runMessage msg l@(NarrowPass a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [(t, n) | Window _ (DealtDamage t n) <- windows] \(t, n) -> do
        cancelDamage t
        removeWainridersDamage n
      pure l
    _ -> NarrowPass <$> runMessage msg a
