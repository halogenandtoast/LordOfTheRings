module LordOfTheRings.Location.Cards.AnnuminasCountryside
  (gateOfAnnuminas, arnorBattlefield, hillsOfEvendim, shoresOfLakeNenuial) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (stagedEnemyEngagements, canAssignDistinct, damageableControlledHeroes)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AnnuminasCountryside = AnnuminasCountryside LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfAnnuminas, arnorBattlefield, hillsOfEvendim, shoresOfLakeNenuial :: LocationCard AnnuminasCountryside
gateOfAnnuminas = location AnnuminasCountryside Cards.gateOfAnnuminas
arnorBattlefield = location AnnuminasCountryside Cards.arnorBattlefield
hillsOfEvendim = location AnnuminasCountryside Cards.hillsOfEvendim
shoresOfLakeNenuial = location AnnuminasCountryside Cards.shoresOfLakeNenuial

-- A travel cost must be payable by every player, with a different enemy for
-- each engagement. Counting enemies alone misses player-specific restrictions.
canPayEngagements :: [(PlayerId, [EnemyId])] -> Bool
canPayEngagements = canAssignDistinct . map snd

instance HasModifiersFor AnnuminasCountryside where
  getModifiersFor (AnnuminasCountryside a)
    | code == toCardCode Cards.gateOfAnnuminas = do
        enemies <- select AnyEnemy
        modifySelf a [ThreatModifier $ length enemies + 1, TravelRequiresEachPlayerEngagement]
    | code == toCardCode Cards.arnorBattlefield = do
        when (a.placement == StagingArea) $
          select (not_ UniqueEnemy) >>= traverse_ (\eid -> modified a eid [EngagementCostModifier (-5)])
        modifySelf a [TravelRequiresEachPlayerHeroDamage]
    | code == toCardCode Cards.hillsOfEvendim = do
        when (a.placement == StagingArea) $ modified a GameTarget [WhenRevealedCannotBeCanceled]
        available <- getsGame $ \g -> let scenario = toAttrs g.scenario in scenario.encounterDeck <> scenario.encounterDiscard
        unless (any (cardMatches $ CardWithType EnemyType) available) $ modifySelf a [CannotBeTraveledTo]
    | otherwise = when (a.placement == StagingArea) $
        select (LocationInStagingArea <> not_ (LocationWithId a.id)) >>= traverse_ (\lid -> modified a lid [ProgressLimit 0])
   where
    code = toCardCode a

instance RunMessage AnnuminasCountryside where
  runMessage msg l@(AnnuminasCountryside a) = case msg of
    TravelTo lid | lid == a.id -> do
      if
        | toCardCode a == toCardCode Cards.gateOfAnnuminas -> eachPlayer $ \pid -> push $ WithTarget (PlayerTarget pid) $ DoStep 2 msg
        | toCardCode a == toCardCode Cards.arnorBattlefield -> eachPlayer $ \pid -> do
            heroes <- damageableControlledHeroes (toSource a) pid
            chooseOrRunOneM pid do
              questionLabeled "annuminas.battlefieldHero"
              causedBy a
              targets heroes $ \hid -> dealDamage a hid 1
        | toCardCode a == toCardCode Cards.hillsOfEvendim -> do
            shuffleEncounterDiscardIntoDeck AnyCard
            doStep 2 msg
        | otherwise -> eachPlayer (`encounterRaisesThreat` 2)
      pure l
    WithTarget (PlayerTarget pid) (DoStep 2 (TravelTo lid)) | lid == a.id -> do
      candidates <- stagedEnemyEngagements =<< getRemainingPlayers
      case dropWhile ((/= pid) . fst) candidates of
        (_, enemies) : remaining -> do
          let eligible = [eid | eid <- enemies, canPayEngagements [(owner, filter (/= eid) options) | (owner, options) <- remaining]]
          chooseOrRunOneM pid do
            questionLabeled "annuminas.gateEngage"
            causedBy a
            targets eligible (engageEnemy pid)
        _ -> pure ()
      pure l
    DoStep 2 (TravelTo lid) | lid == a.id -> l <$ discardEncounterCardsUntil a (CardWithType EnemyType)
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches $ CardWithType EnemyType) cards
      pure l
    _ -> AnnuminasCountryside <$> runMessage msg a
