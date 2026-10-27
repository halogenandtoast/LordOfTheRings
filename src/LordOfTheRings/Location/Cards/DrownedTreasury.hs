module LordOfTheRings.Location.Cards.DrownedTreasury (drownedTreasury) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))

newtype DrownedTreasury = DrownedTreasury LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drownedTreasury :: LocationCard DrownedTreasury
drownedTreasury = location DrownedTreasury Cards.drownedTreasury

-- | If Drowned Treasury is the active location at the end of the quest phase, each player
-- must discard 1 character he controls.
-- Response: After Drowned Treasury leaves play as an explored location, each player may
-- draw 2 cards or claim 1 objective in play.
instance HasAbilities DrownedTreasury where
  getAbilities (DrownedTreasury a) =
    [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]
      <> [mkAbility a 2 $ response $ ExploredWindow When a.id]

instance RunMessage DrownedTreasury where
  runMessage msg l@(DrownedTreasury a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> chooseAndDiscard a pid =<< charactersControlledBy pid
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \pid -> do
        -- an objective in play no one has claimed waits in the staging area
        objectives <-
          getsGame (map toId . filter ((== StagingArea) . (.placement)) . toList . (.entities.objectives))
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOneM pid do
          questionLabeled "drownedTreasury.choose"
          causedBy a
          labeled "drownedTreasury.draw" $ drawCards pid 2
          unless (null heroes) $ targets objectives \o ->
            chooseOrRunOneM pid do
              questionLabeled "drownedTreasury.hero"
              causedBy a
              targets heroes (attachObjective o)
          labeled "pass" $ pure ()
      pure l
    _ -> DrownedTreasury <$> runMessage msg a
