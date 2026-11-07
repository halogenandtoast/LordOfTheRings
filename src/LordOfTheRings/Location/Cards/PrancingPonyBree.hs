module LordOfTheRings.Location.Cards.PrancingPonyBree
  (thePrancingPonyMurder, ramshackleManor, darkAlleyMurder, marketSquareMurder, oldWarehouse, villageStable) where

import LordOfTheRings.Card (toCardCode, CardType (EnemyType))
import LordOfTheRings.Helpers.Window (playerCardController)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (lookAtInvestigation)
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait (Trait (Suspect, Hideout))
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype PrancingPonyBree = PrancingPonyBree LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

thePrancingPonyMurder, ramshackleManor, darkAlleyMurder, marketSquareMurder, oldWarehouse, villageStable :: LocationCard PrancingPonyBree
thePrancingPonyMurder = location PrancingPonyBree Cards.thePrancingPonyMurder
ramshackleManor = location PrancingPonyBree Cards.ramshackleManor
darkAlleyMurder = location PrancingPonyBree Cards.darkAlleyMurder
marketSquareMurder = location PrancingPonyBree Cards.marketSquareMurder
oldWarehouse = location PrancingPonyBree Cards.oldWarehouse
villageStable = location PrancingPonyBree Cards.villageStable

instance HasModifiersFor PrancingPonyBree where
  getModifiersFor (PrancingPonyBree a) = do
    when (toCardCode a == toCardCode Cards.ramshackleManor) $ modified a GameTarget [PlayerCardThreatReductionLimit 1]
    when (toCardCode a == toCardCode Cards.darkAlleyMurder) $
      select EnemyInStagingArea >>= traverse_ (\eid -> modified a eid [CannotTakeDamage])
    when (toCardCode a == toCardCode Cards.marketSquareMurder) do
      players <- getRemainingPlayers
      pools <- traverse (`getResourcesOfSphere` Neutral) players
      when (any (< 1) pools) $ modifySelf a [CannotBeTraveledTo]
    when (toCardCode a == toCardCode Cards.villageStable) do
      players <- getRemainingPlayers
      hands <- map discardableHand <$> traverse getPlayer players
      blocked <- anyM (\pid -> elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)) players
      when (blocked || any null hands) $ modifySelf a [CannotBeTraveledTo]

instance HasAbilities PrancingPonyBree where
  getAbilities (PrancingPonyBree a)
    | toCardCode a == toCardCode Cards.thePrancingPonyMurder = [mkAbility a 1 $ forced $ ExploredWindow When a.id]
    | toCardCode a == toCardCode Cards.marketSquareMurder = [mkAbility a 2 $ forced $ PlayerCardPlacedProgressWindow After LocationInStagingArea]
    | toCardCode a == toCardCode Cards.oldWarehouse = [mkAbility a 3 $ forced $ InvestigateWindow When a.id]
    | toCardCode a == toCardCode Cards.villageStable = [mkAbility a 4 $ response $ InvestigatedMatchingCardWindow a.id (CardMatchAny [CardWithTrait Suspect, CardWithTrait Hideout])]
    | otherwise = []

instance RunMessage PrancingPonyBree where
  runMessage msg l@(PrancingPonyBree a) = case msg of
    TravelTo lid | lid == a.id -> do
      if
        | toCardCode a == toCardCode Cards.ramshackleManor -> eachPlayer (`encounterRaisesThreat` 1)
        | toCardCode a == toCardCode Cards.darkAlleyMurder -> shuffleEncounterDiscardIntoDeck AnyCard >> doStep 1 msg
        | toCardCode a == toCardCode Cards.marketSquareMurder -> eachPlayer $ \pid -> spendResources pid Neutral 1
        | toCardCode a == toCardCode Cards.oldWarehouse -> revealEncounterCard
        | toCardCode a == toCardCode Cards.villageStable -> eachPlayer $ \pid -> discardRandomFromHand pid 1
        | otherwise -> pure ()
      pure l
    DoStep 1 (TravelTo lid) | lid == a.id -> l <$ discardEncounterCardsUntil a (CardWithType EnemyType)
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches $ CardWithType EnemyType) cards
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "prancingPony.pony"
        causedBy a
        labeled "prancingPony.lookTwo" $ lookAtInvestigation lead a 2
        labeled "prancingPony.drawTwoEach" $ eachPlayer $ \pid -> drawCards a pid 2
      pure l
    UseCardAbility _ (isSource a -> True) 2 ws -> do
      for_ [source | Window After (ProgressPlacedOnLocationBy source _) <- ws] $ \source ->
        traverse_ (\pid -> encounterRaisesThreat pid 1) =<< playerCardController source
      pure l
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "prancingPony.warehouse"
        causedBy a
        labeled "prancingPony.lookAdditionalTwo" $ createEffect a a [AdditionalInvestigateCards 2] UntilSpent
        labeled "prancingPony.reduceThreeEach" $ eachPlayer $ \pid -> lowerThreatBy a pid 3
      pure l
    UseCardAbility _ (isSource a -> True) 4 ws -> do
      lead <- getLead
      let cards = [card | Window After (Investigated lid looked) <- ws, lid == a.id, card <- looked, cardMatches (CardMatchAny [CardWithTrait Suspect, CardWithTrait Hideout]) card]
      chooseOrRunOneM lead do
        questionLabeled "prancingPony.stageClue"
        causedBy a
        targets cards $ \card -> do
          addToVictoryDisplay a
          push $ StageInvestigationCard card
      pure l
    _ -> PrancingPonyBree <$> runMessage msg a
