module LordOfTheRings.Quest.Cards.SurpriseAttack (surpriseAttack) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (ObjectiveHeroType), EncounterCard, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (isObjectiveHero, stagingHeroRules, takeHeroFromStagingArea)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEncounterDeck, scenarioSetAside))

newtype SurpriseAttack = SurpriseAttack QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

surpriseAttack :: QuestCard SurpriseAttack
surpriseAttack = quest SurpriseAttack Cards.surpriseAttack

-- | Damage from undefended attacks must be dealt to a hero in the staging area, if able.
-- The first card each player plays each phase does not require a resource match.
instance HasModifiersFor SurpriseAttack where
  getModifiersFor (SurpriseAttack a) = when a.flipped $ stagingHeroRules a

-- | Forced: After placing progress from questing successfully, each player chooses a hero
-- he owns in the staging area, takes control of that hero, and exhausts it. Advance to
-- stage 2A.
instance HasAbilities SurpriseAttack where
  getAbilities (SurpriseAttack a) = [mkAbility a 1 $ forced $ PlacedProgressFromQuestingWindow After | a.flipped]

{- | Setup: Set Uruk Chieftain aside, out of play. Each player adds 1 copy of Sauron's
Enforcers to the staging area. Move each hero to the staging area. Each player takes
control of an objective-hero and adds 2 resources to its resource pool. Remove each
remaining objective-hero from the game. For the remainder of the game, heroes in the
staging area are in play but under no player's control, immune to player card effects,
and their text boxes are considered to be blank (the scenario keeps that lasting effect).

The scenario set Uruk Chieftain and the objective-heroes aside as it gathered the
encounter deck. Starting with the first player, each player chooses 1 of the objective-
heroes left, in turn order (MEC55 rules).
-}
instance RunMessage SurpriseAttack where
  runMessage msg q@(SurpriseAttack attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      players <- getRemainingPlayers
      deck <- getsGame (scenarioEncounterDeck . toAttrs . (.scenario))
      for_ (take (length players) $ filter (cardMatches $ cardIs Enemies.sauronsEnforcers) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      heroes <- select AnyHero
      for_ heroes moveHeroToStagingArea
      aside <- getsGame (filter ((== ObjectiveHeroType) . cdCardType . toCardDef) . scenarioSetAside . toAttrs . (.scenario))
      pickObjectiveHeroes players aside
      doStep 1 msg
      SurpriseAttack <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      objectiveHeroes <- filterM (isObjectiveHero . toTarget) =<< select AnyHero
      for_ objectiveHeroes \h -> placeResources attrs h 2
      aside <- getsGame (filter ((== ObjectiveHeroType) . cdCardType . toCardDef) . scenarioSetAside . toAttrs . (.scenario))
      for_ aside \c -> removeFromGame (CardIdTarget c.id)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer $ takeHeroFromStagingArea attrs
      advanceQuest
      pure q
    _ -> SurpriseAttack <$> runMessage msg attrs
   where
    pickObjectiveHeroes :: ReverseQueue m => [PlayerId] -> [EncounterCard] -> m ()
    pickObjectiveHeroes (p : ps) available@(_ : _) = chooseOrRunOneM p do
      questionLabeled "surpriseAttack.objectiveHero"
      causedBy attrs
      showing (map EncounterCard available)
      targets available \c -> do
        putObjectiveHeroIntoPlayUnder p c
        pickObjectiveHeroes ps (filter (/= c) available)
    pickObjectiveHeroes _ _ = pure ()
