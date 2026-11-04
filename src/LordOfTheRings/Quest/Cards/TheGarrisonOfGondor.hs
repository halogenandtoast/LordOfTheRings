module LordOfTheRings.Quest.Cards.TheGarrisonOfGondor (theGarrisonOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (genPlayerCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype TheGarrisonOfGondor = TheGarrisonOfGondor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theGarrisonOfGondor :: QuestCard TheGarrisonOfGondor
theGarrisonOfGondor = quest TheGarrisonOfGondor Cards.theGarrisonOfGondor

-- | Skip the quest phase and combat phase.
instance HasModifiersFor TheGarrisonOfGondor where
  getModifiersFor (TheGarrisonOfGondor a) = when a.flipped $ modified a GameTarget [SkipQuestPhase, SkipCombatPhase]

-- | Forced: At the end of the planning phase, place 1 progress here.
instance HasAbilities TheGarrisonOfGondor where
  getAbilities (TheGarrisonOfGondor a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Set Minas Tirith, Grond, The Witch-king, and each copy of Wraith on Wings aside,
out of play (the scenario does as it gathers). Search the encounter deck for a copy of
Pelennor Field and make it the active location. Shuffle the encounter deck. The players may
attach Esquire of Rohan and Esquire of Gondor to different heroes: the first player
chooses, each under the control of its hero's controller.
-}
instance RunMessage TheGarrisonOfGondor where
  runMessage msg q@(TheGarrisonOfGondor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame (scenarioEncounterDeck . toAttrs . (.scenario))
      for_ (find ((== toCardCode Locations.pelennorField) . toCardCode) deck) \card -> do
        removeFromEncounterDeck card
        putIntoPlayAsActiveLocation card
      shuffleEncounterDeck
      doStep 1 msg
      TheGarrisonOfGondor <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      esquire "esquireOfRohan" Attachments.esquireOfRohan
      doStep 2 msg
      pure q
    DoStep 2 (PerformSetup (isTarget attrs -> True)) -> do
      esquire "esquireOfGondor" Attachments.esquireOfGondor
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 1
      pure q
    _ -> TheGarrisonOfGondor <$> runMessage msg attrs
   where
    -- a hero with the other Esquire already attached takes none
    esquire key def = do
      lead <- getLead
      let esquires = map toCardCode [Attachments.esquireOfRohan, Attachments.esquireOfGondor]
      taken <- getsGame (\g -> [a.placement | a <- toList g.entities.attachments, toCardCode a `elem` esquires])
      heroes <- filter (\h -> AttachedTo (HeroTarget h.id) `notElem` taken) <$> (traverse getHero =<< select AnyHero)
      chooseOneM lead do
        questionLabeled ("theGarrisonOfGondor." <> key)
        for_ heroes \h -> targeting h.id do
          card <- genPlayerCard def (Just h.owner)
          attachOutOfPlay h.owner card h.id
        labeled "pass" $ pure ()
