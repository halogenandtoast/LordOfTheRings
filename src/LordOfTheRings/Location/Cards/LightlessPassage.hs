module LordOfTheRings.Location.Cards.LightlessPassage (lightlessPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch)

newtype LightlessPassage = LightlessPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lightlessPassage :: LocationCard LightlessPassage
lightlessPassage = location LightlessPassage Cards.lightlessPassage

-- | Travel: Players must exhaust a Cave Torch to travel here.
instance HasModifiersFor LightlessPassage where
  getModifiersFor (LightlessPassage a) = do
    torch <- readyCaveTorch
    when (isNothing torch) $ modified a a [CannotBeTraveledTo]

-- | Shadow: Cancel all combat damage dealt to attacking enemy.
instance RunMessage LightlessPassage where
  runMessage msg l@(LightlessPassage a) = case msg of
    TravelTo lid | lid == a.id -> do
      readyCaveTorch >>= traverse_ (exhaust . ObjectiveTarget . fst)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      phaseModifier a eid CannotTakeCombatDamage
      pure l
    _ -> LightlessPassage <$> runMessage msg a
