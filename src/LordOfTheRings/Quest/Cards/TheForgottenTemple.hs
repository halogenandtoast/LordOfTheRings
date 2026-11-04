module LordOfTheRings.Quest.Cards.TheForgottenTemple (theForgottenTemple) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheForgottenTemple = TheForgottenTemple QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theForgottenTemple :: QuestCard TheForgottenTemple
theForgottenTemple = quest TheForgottenTemple Cards.theForgottenTemple

-- | Skip the quest phase.
instance HasModifiersFor TheForgottenTemple where
  getModifiersFor (TheForgottenTemple a) = when a.flipped $ modified a GameTarget [SkipQuestPhase]

instance HasAbilities TheForgottenTemple where
  getAbilities (TheForgottenTemple a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Shuffle each of the stage 2 quest cards together and place them facedown under
this stage. Create The Power of Mordor deck and set it next to the quest deck. Add Thane
Ulchor and Tribute to Mordor to the staging area. Shuffle the encounter deck. (The
scenario builds the quest deck and sets the two cards aside as it gathers.)

Forced: At the end of the planning phase, place 1 resource token here. Then, if there are
2 resource tokens here, add this stage to the victory display and advance to the topmost
stage 2A.
-}
instance RunMessage TheForgottenTemple where
  runMessage msg q@(TheForgottenTemple attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      createPowerOfMordorDeck
      addSetAsideToStaging $ CardMatchAny [cardIs Enemies.thaneUlchorTheTempleOfDoom, cardIs Cards.tributeToMordor]
      shuffleEncounterDeck
      TheForgottenTemple <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      when (countTokens Resource attrs.questTokens >= 2) do
        addToVictoryDisplay attrs
        advanceQuest
      pure q
    _ -> TheForgottenTemple <$> runMessage msg attrs
