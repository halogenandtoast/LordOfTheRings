module LordOfTheRings.Quest.Cards.BelegostDepths
  ( theServantsOfMorgoth, theRootsOfEredLuin, theMinesOfBelegost, theBeastOfBelegost ) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers
import LordOfTheRings.Trait (Trait (Hazard))

newtype BelegostDepths = BelegostDepths QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theServantsOfMorgoth, theRootsOfEredLuin, theMinesOfBelegost, theBeastOfBelegost :: QuestCard BelegostDepths
theServantsOfMorgoth = quest BelegostDepths Cards.theServantsOfMorgoth
theRootsOfEredLuin = quest BelegostDepths Cards.theRootsOfEredLuin
theMinesOfBelegost = quest BelegostDepths Cards.theMinesOfBelegost
theBeastOfBelegost = quest BelegostDepths Cards.theBeastOfBelegost

instance HasModifiersFor BelegostDepths where
  getModifiersFor (BelegostDepths a) = when a.flipped do
    let code = toCardCode a
    when (code /= toCardCode Cards.theBeastOfBelegost) $ modified a GameTarget [MustTravel]
    when (code == toCardCode Cards.theServantsOfMorgoth) $ modified a GameTarget [FirstRevealedCardWithTraitGainsKeywords Hazard [Surge]]
    when (code == toCardCode Cards.theRootsOfEredLuin) do
      active <- select IsActiveLocation
      for_ active \lid -> modified a lid [DiscoverValueModifier 1]
    when (code == toCardCode Cards.theBeastOfBelegost) $ modifySelf a [CannotBeDefeated]

instance HasAbilities BelegostDepths where
  getAbilities (BelegostDepths a) =
    [restricted a 1 (TargetHasModifier GameTarget AttackWasDealtBlankShadow) $ forced $ MatchingEnemyAttacksWindow After (EnemyWithCardCode $ toCardCode Enemies.naurlhug) | a.flipped, toCardCode a == toCardCode Cards.theBeastOfBelegost]
    <> [mkAbility a 2 $ forced $ DiscoverResolvedWithoutHazardWindow After | a.flipped, toCardCode a == toCardCode Cards.theMinesOfBelegost]

instance RunMessage BelegostDepths where
  runMessage msg q@(BelegostDepths a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      BelegostDepths <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      if toCardCode a == toCardCode Cards.theBeastOfBelegost then do
        summonNaurlhug
        addSetAsideToStaging (cardIs Locations.naurlhugsLair)
        -- Sword and the discard are shuffled only after both unique cards enter.
        doStep 1 msg
      else do
        shuffleEncounterDiscardIntoDeck AnyCard
        revealEncounterCards . length =<< getPlayerOrder
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      aside <- getsGame ((.scenarioSetAside) . toAttrs . (.scenario))
      shuffleSetAsideIntoEncounterDeck $ filter ((== toCardCode Objectives.swordOfBelegost) . toCardCode) aside
      shuffleEncounterDiscardIntoDeck AnyCard
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> q <$ revealEncounterCard
    CheckWindows windows | a.flipped, toCardCode a == toCardCode Cards.theBeastOfBelegost -> do
      -- The enemy has left play by the after window: use its defeated card.
      when (any (\case Window After (EnemyDestroyed card) -> toCardCode card == toCardCode Enemies.naurlhug; _ -> False) windows) winGame
      BelegostDepths <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [(eid, pid) | Window After (EnemyAttacking eid pid) <- windows] \(eid, pid) -> do
        -- FAQ 1.46: a solo game has no next player; "then" requires the engagement.
        next <- getNextPlayer pid
        for_ next \player -> do
          engageEnemy player eid
          immediateAttackWithoutShadow player eid
      pure q
    _ -> BelegostDepths <$> runMessage msg a
