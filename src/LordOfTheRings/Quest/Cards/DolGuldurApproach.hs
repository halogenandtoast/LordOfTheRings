module LordOfTheRings.Quest.Cards.DolGuldurApproach (assaultOnTheGoldenWood, crossingTheAnduinDolGuldur) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (EpicEffect (EpicDolDrawResource))
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (chooseDolGuldurStage)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Trait (Trait (DolGuldur, Lieutenant))

newtype DolGuldurApproach = DolGuldurApproach QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

assaultOnTheGoldenWood, crossingTheAnduinDolGuldur :: QuestCard DolGuldurApproach
assaultOnTheGoldenWood = quest DolGuldurApproach Cards.assaultOnTheGoldenWood
crossingTheAnduinDolGuldur = quest DolGuldurApproach Cards.crossingTheAnduinDolGuldur

instance HasAbilities DolGuldurApproach where
  getAbilities (DolGuldurApproach a) =
    [ability | a.flipped, toCardCode a == toCardCode Cards.assaultOnTheGoldenWood
    , ability <- [mkAbility a 1 $ forced RoundEndsWindow, mkAbility a 2 $ ResponseAbility RoundEndsWindow $ EachPlayerThreatCost 1]]

instance RunMessage DolGuldurApproach where
  runMessage msg q@(DolGuldurApproach a) = case msg of
    DoStep 7 Setup | toCardCode a == toCardCode Cards.assaultOnTheGoldenWood -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (filter (\card -> cardMatches (CardWithTrait Lieutenant) card || toCardCode card == toCardCode Objectives.hostOfLorien) deck) setAside
      for_ (find ((== toCardCode Objectives.powerOfLorien) . toCardCode) deck) $ \card -> do
        removeFromEncounterDeck card
        addToStaging card
      players <- getRemainingPlayers
      for_ (take (length players) $ filter ((== toCardCode Enemies.dolGuldurMarauder) . toCardCode) deck) $ \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      turnOver a
      pure q
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      when (toCardCode a == toCardCode Cards.crossingTheAnduinDolGuldur) $ askToResolveWhenRevealed a
      DolGuldurApproach <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      power <- getsGame $ map toAttrs . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)
      lead <- getLead
      if null power then doStep 1 msg else chooseOrRunOneM lead do
        causedBy a
        targets power $ \oid -> do
          -- Apply FAQ 1.34's singular-target precedent when several copies match.
          removeTokens a a Progress a.progress
          placeProgress a oid a.progress
          doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Resource a.questTokens >= 3) advanceQuest
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      lead <- getLead
      chooseDolGuldurStage msg lead a EpicDolDrawResource
      pure q
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      power <- getsGame $ map toAttrs . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)
      if null power then doStep 2 msg else chooseOrRunOneM lead do
        causedBy a
        targets power $ \oid -> push $ WithTarget (toTarget oid) $ DoStep 3 msg
      pure q
    WithTarget (ObjectiveTarget oid) (DoStep 3 (WhenRevealed (isTarget a -> True))) -> do
      lead <- getLead
      aside <- getsGame $ scenarioSetAside . toAttrs . (.scenario)
      power <- getsGame $ map toAttrs . filter ((== oid) . toId) . toList . (.entities.objectives)
      case power of
        [] -> doStep 2 $ WhenRevealed $ toTarget a
        chosen : _ -> do
          let progress = countTokens Progress chosen.objectiveTokens
              host = find ((== toCardCode Objectives.hostOfLorien) . toCardCode) aside
          if progress >= 10 && isJust host then chooseOneM lead do
            questionLabeled "dolGuldur.claimHost"
            causedBy a
            labeled "dolGuldur.takeHost" do
              removeTokens a oid Progress 10
              traverse_ (putObjectiveAllyIntoPlayUnder lead) host
              -- FAQ 1.15: taking the Host must resolve before the "Then".
              push $ WithTarget (ObjectiveTarget oid) $ DoStep 1 $ WhenRevealed $ toTarget a
            labeled "skip" do
              discard a oid
              push $ DoStep 2 $ WhenRevealed $ toTarget a
          else do
            discard a oid
            doStep 2 $ WhenRevealed $ toTarget a
      pure q
    WithTarget (ObjectiveTarget selected) (DoStep 1 (WhenRevealed (isTarget a -> True))) -> do
      lead <- getLead
      power <- getsGame $ map toAttrs . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)
      for_ (find ((== selected) . toId) power) $ \oid -> do
        let progress = countTokens Progress oid.objectiveTokens
        if progress >= 20 then chooseOneM lead do
          questionLabeled "dolGuldur.claimPower"
          causedBy a
          labeled "dolGuldur.takePower" do
            removeTokens a oid Progress 20
            push $ ClaimObjective (toId oid) $ PlayerTarget lead
          labeled "dolGuldur.discardPower" $ discard a oid
        else discard a oid
      doStep 2 $ WhenRevealed $ toTarget a
      pure q
    DoStep 2 (WhenRevealed (isTarget a -> True)) -> do
      eachPlayer $ \pid -> searchEncounterDeckAndDiscard pid a $ CardWithType LocationType <> CardWithTrait DolGuldur
      shuffleEncounterDeck
      push $ ScenarioSpecific "dolGuldur.chooseStage" Null
      pure q
    _ -> DolGuldurApproach <$> runMessage msg a
