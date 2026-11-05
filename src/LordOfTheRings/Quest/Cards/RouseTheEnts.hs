module LordOfTheRings.Quest.Cards.RouseTheEnts (rouseTheEnts) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RouseTheEnts = RouseTheEnts QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

rouseTheEnts :: QuestCard RouseTheEnts
rouseTheEnts = quest RouseTheEnts Cards.rouseTheEnts

{- | Players cannot draw cards or gain resources. Do not commit characters to the quest or
reveal encounter cards during the quest phase: no staging step, so the quest is resolved
with nothing committed against the staging area's threat.
-}
instance HasModifiersFor RouseTheEnts where
  getModifiersFor (RouseTheEnts a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDrawCards, CannotCommitCharacters]
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotCollectResources, CannotGainResourcesFromCardEffects]
    modified a GameTarget [SkipStagingStep]

-- | Forced: At the end of the round, place 1 resource here. Then, the players may advance
-- to stage 2.
instance HasAbilities RouseTheEnts where
  getAbilities (RouseTheEnts a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Set Gates of Isengard, Orthanc, Saruman, and Saruman's Staff aside, out of play.
Add each copy of Ent of Fangorn to the staging area. Shuffle the encounter deck. (The
scenario sets them aside as it gathers the deck, the Ents with them.)

If the players advance, the first player shuffles the encounter discard pile into the
encounter deck and reveals 1 encounter card for each resource here. Then, remove each Ent
objective in the staging area from the game. The first player decides for the group.
-}
instance RunMessage RouseTheEnts where
  runMessage msg q@(RouseTheEnts attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Objectives.entOfFangorn)
      shuffleEncounterDeck
      RouseTheEnts <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "rouseTheEnts.advance"
        causedBy attrs
        labeled "rouseTheEnts.advanceToStage2" do
          shuffleEncounterDiscardIntoDeck AnyCard
          asRevealer lead $ revealEncounterCards (countTokens Resource attrs.questTokens)
          ents <- getsGame (filter (\o -> toCardCode o == toCardCode Objectives.entOfFangorn && o.placement == StagingArea) . toList . (.entities.objectives))
          for_ ents removeFromGame
          advanceQuest
        labeled "rouseTheEnts.stay" (pure ())
      pure q
    _ -> RouseTheEnts <$> runMessage msg attrs
