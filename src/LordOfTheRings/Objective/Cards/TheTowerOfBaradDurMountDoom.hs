module LordOfTheRings.Objective.Cards.TheTowerOfBaradDurMountDoom (theTowerOfBaradDurMountDoom) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype TheTowerOfBaradDurMountDoom = TheTowerOfBaradDurMountDoom ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theTowerOfBaradDurMountDoom :: ObjectiveCard TheTowerOfBaradDurMountDoom
theTowerOfBaradDurMountDoom = objective TheTowerOfBaradDurMountDoom Cards.theTowerOfBaradDurMountDoom

-- | Players do not raise their threat at the end of the refresh phase.
instance HasModifiersFor TheTowerOfBaradDurMountDoom where
  getModifiersFor (TheTowerOfBaradDurMountDoom a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [RefreshThreatRaise 0]

-- | Forced: At the end of the round, each player raises his threat by 1 for each
-- non-Fellowship character he controls.
instance HasAbilities TheTowerOfBaradDurMountDoom where
  getAbilities (TheTowerOfBaradDurMountDoom a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage TheTowerOfBaradDurMountDoom where
  runMessage msg o@(TheTowerOfBaradDurMountDoom attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> do
        characters <-
          selectCharacters
            (HeroControlledBy (PlayerWithId pid) <> not_ (HeroWithSphere Fellowship))
            (AllyControlledBy (PlayerWithId pid) <> not_ (AllyWithSphere Fellowship))
        when (notNull characters) $ encounterRaisesThreat pid (length characters)
      pure o
    _ -> TheTowerOfBaradDurMountDoom <$> runMessage msg attrs
