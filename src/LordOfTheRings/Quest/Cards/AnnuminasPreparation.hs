module LordOfTheRings.Quest.Cards.AnnuminasPreparation
  (rebuildTheDefenses, raiseTheLevee, delayTheEnemyAnnuminas) where

import LordOfTheRings.Card (CardType (LocationType, EnemyType), toCardCode)
import LordOfTheRings.Epic.Types (EpicEffect (EpicGrantDunedain, EpicDelayStaging))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (repairAnnuminas, setAsideAnnuminasCards, eachAnnuminasTeam, otherAnnuminasTeams)

newtype AnnuminasPreparation = AnnuminasPreparation QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rebuildTheDefenses, raiseTheLevee, delayTheEnemyAnnuminas :: QuestCard AnnuminasPreparation
rebuildTheDefenses = quest AnnuminasPreparation Cards.rebuildTheDefenses
raiseTheLevee = quest AnnuminasPreparation Cards.raiseTheLevee
delayTheEnemyAnnuminas = quest AnnuminasPreparation Cards.delayTheEnemyAnnuminas

instance HasAbilities AnnuminasPreparation where
  getAbilities (AnnuminasPreparation a)
    | a.flipped =
        [mkAbility a 1 $ forced RoundBeginsWindow]
          <> if toCardCode a == toCardCode Cards.rebuildTheDefenses
            then [mkAbility a 2 $ ResponseAbility RoundEndsWindow (QuestProgressCost a.id 10)]
            else [mkAbility a 2 $ response RoundEndsWindow | a.progress > 0]
    | otherwise = []

instance RunMessage AnnuminasPreparation where
  runMessage msg q@(AnnuminasPreparation a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      setAsideAnnuminasCards
      if toCardCode a == toCardCode Cards.raiseTheLevee
        then do
          shuffleEncounterDeck
          doStep 1 msg
        else do
          players <- getPlayerOrder
          let (required, kind) = if toCardCode a == toCardCode Cards.rebuildTheDefenses
                then (toCardCode Locations.gateOfAnnuminas, LocationType)
                else (toCardCode Enemies.vanguardOfCarnDum, EnemyType)
          push $ SearchDifferentEncounterCardsIncluding required players (toTarget a) (CardWithType kind) False []
          turnOver a
      pure q
    DoStep n inner@(PerformSetup (isTarget a -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          discardEncounterCardsUntilFor a (PlayerTarget pid) enemyOrLocation
          doStep (n + 1) inner
        [] -> turnOver a
      pure q
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget _) cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches enemyOrLocation) cards
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Resource a.questTokens >= 4) advanceQuest
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      if toCardCode a == toCardCode Cards.rebuildTheDefenses
        then repairAnnuminas a 10
        else do
          -- FAQ 1.15: the effect before "then" must resolve successfully.
          removeTokens a a Progress 10
          when (a.progress >= 10) $
            if toCardCode a == toCardCode Cards.raiseTheLevee
              then eachAnnuminasTeam (toCardCode a) EpicGrantDunedain
              else otherAnnuminasTeams (toCardCode a) EpicDelayStaging
      pure q
    _ -> AnnuminasPreparation <$> runMessage msg a

enemyOrLocation :: CardMatcher
enemyOrLocation = CardMatchAny [CardWithType EnemyType, CardWithType LocationType]
