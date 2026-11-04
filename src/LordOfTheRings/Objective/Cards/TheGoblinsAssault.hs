module LordOfTheRings.Objective.Cards.TheGoblinsAssault (theGoblinsAssault) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (getUrdug)

newtype TheGoblinsAssault = TheGoblinsAssault ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theGoblinsAssault :: ObjectiveCard TheGoblinsAssault
theGoblinsAssault = objective TheGoblinsAssault Cards.theGoblinsAssault

{- | Urdug gets +2 defense for each other enemy in play and cannot be dealt damage unless he
has resolved an attack this phase. The game marks an enemy with 'AttackedThisPhase' as its
attack begins; Urdug being immune to player card effects, only the players' attacks, made
after his, could deal him damage, so it reads the same. The mark is read off the game's
effects, not his modifiers, which this adds to.
-}
instance HasModifiersFor TheGoblinsAssault where
  getModifiersFor (TheGoblinsAssault a) =
    getUrdug >>= traverse_ \u -> do
      others <- length <$> select (not_ $ EnemyWithId u)
      hasAttacked <- getsGame (any (\e -> effectTarget e == EnemyTarget u && AttackedThisPhase `elem` effectModifiers e) . gameEffects)
      modified a u $ [DefenseModifier (2 * others) | others > 0] <> [CannotTakeDamage | not hasAttacked]

-- | Forced: After the players quest unsuccessfully, place 1 progress here for each player in
-- the game.
instance HasAbilities TheGoblinsAssault where
  getAbilities (TheGoblinsAssault a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After]

instance RunMessage TheGoblinsAssault where
  runMessage msg o@(TheGoblinsAssault attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      placeProgress attrs attrs players
      pure o
    _ -> TheGoblinsAssault <$> runMessage msg attrs
