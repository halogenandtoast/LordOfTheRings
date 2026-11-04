module LordOfTheRings.Enemy.Cards.MirkwoodPatrol (mirkwoodPatrol) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (objectiveLocationsInPlay)

newtype MirkwoodPatrol = MirkwoodPatrol EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodPatrol :: EnemyCard MirkwoodPatrol
mirkwoodPatrol = enemy MirkwoodPatrol Cards.mirkwoodPatrol

-- | Forced: At the end of the encounter phase, if Mirkwood Patrol is in the staging area
-- with an unguarded objective-location, attach that objective-location to Mirkwood
-- Patrol, guarding it.
instance HasAbilities MirkwoodPatrol where
  getAbilities (MirkwoodPatrol a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase | a.placement == StagingArea]

{- | With several unguarded objective-locations in the staging area, the first player
chooses the one it guards.
Shadow: The defender cannot ready this round.
-}
instance RunMessage MirkwoodPatrol where
  runMessage msg e@(MirkwoodPatrol attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      staged <- select LocationInStagingArea
      unguarded <- filter (`elem` staged) <$> objectiveLocationsInPlay
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "mirkwoodPatrol.guard"
        causedBy attrs
        targets unguarded \l -> placeLocation l (AttachedTo $ CardIdTarget attrs.cardId)
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier (CardIdSource cid) d CannotReady
      pure e
    _ -> MirkwoodPatrol <$> runMessage msg attrs
