module LordOfTheRings.Location.Cards.Midgewater (midgewater) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype Midgewater = Midgewater LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

midgewater :: LocationCard Midgewater
midgewater = location Midgewater Cards.midgewater

-- | While Midgewater is the active location, enemies cannot attack, take damage, or be
-- engaged: by engagement check, by choice or by card effect.
instance HasModifiersFor Midgewater where
  getModifiersFor (Midgewater a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    enemies <- select AnyEnemy
    for_ enemies \e ->
      modified a e $ CannotAttack : CannotTakeDamage : CannotBeOptionallyEngagedBy AnyPlayer : map CannotEngagePlayer players

-- | Forced: After Midgewater becomes the active location, return each engaged enemy in play
-- to the staging area.
instance HasAbilities Midgewater where
  getAbilities (Midgewater a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage Midgewater where
  runMessage msg l@(Midgewater a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    _ -> Midgewater <$> runMessage msg a
