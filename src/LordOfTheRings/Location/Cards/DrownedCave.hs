module LordOfTheRings.Location.Cards.DrownedCave (drownedCave) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Undead))

-- | Single-sided, Underwater, in the encounter deck.
newtype DrownedCave = DrownedCave LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

drownedCave :: LocationCard DrownedCave
drownedCave = location DrownedCave Cards.drownedCave

-- | While Drowned Cave is the active location, Undead enemies cannot be engaged (return any
-- engaged Undead enemies to the staging area): those engaged return as it becomes active.
instance HasModifiersFor DrownedCave where
  getModifiersFor (DrownedCave a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    undead <- select (EnemyWithTrait Undead)
    for_ undead \e -> modified a e (map CannotEngagePlayer players)

-- | Forced: If Drowned Cave is the active location at the end of the quest phase, each
-- player must discard the top 10 cards of his deck. Then, you may return Drowned Cave to
-- the staging area.
instance HasAbilities DrownedCave where
  getAbilities (DrownedCave a) =
    mkAbility a 2 (silent $ LocationBecameActiveWindow After (LocationWithId a.id))
      : [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

instance RunMessage DrownedCave where
  runMessage msg l@(DrownedCave a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfDeck p 10
      lead <- getLead
      chooseOneM lead do
        questionLabeled "drownedCave.return"
        causedBy a
        labeled "drownedCave.toStaging" $ placeLocation a.id StagingArea
        labeled "drownedCave.stay" $ pure ()
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      engaged <- select (EnemyWithTrait Undead <> EnemyEngagedWith AnyPlayer)
      for_ engaged \e -> placeEnemy e StagingArea
      pure l
    _ -> DrownedCave <$> runMessage msg a
