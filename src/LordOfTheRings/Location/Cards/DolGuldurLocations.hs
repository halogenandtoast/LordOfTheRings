module LordOfTheRings.Location.Cards.DolGuldurLocations
  (gateOfDolGuldurAttack, theSorcerersTower, theShadowRoad, dungeonDoor, forestBattleground, blackenedWoods) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (EnemyType, EventType), toCardCode, toCardDef, cdThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype DolGuldurLocation = DolGuldurLocation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfDolGuldurAttack, theSorcerersTower, theShadowRoad, dungeonDoor, forestBattleground, blackenedWoods :: LocationCard DolGuldurLocation
gateOfDolGuldurAttack = location DolGuldurLocation Cards.gateOfDolGuldurAttack
theSorcerersTower = location DolGuldurLocation Cards.theSorcerersTower
theShadowRoad = location DolGuldurLocation Cards.theShadowRoad
dungeonDoor = location DolGuldurLocation Cards.dungeonDoor
forestBattleground = location DolGuldurLocation Cards.forestBattleground
blackenedWoods = location DolGuldurLocation Cards.blackenedWoods

instance HasModifiersFor DolGuldurLocation where
  getModifiersFor (DolGuldurLocation a) = do
    let code = toCardCode a
        stages = [(Cards.gateOfDolGuldurAttack, "attack-on-dol-guldur-003"), (Cards.theSorcerersTower, "attack-on-dol-guldur-004"), (Cards.theShadowRoad, "attack-on-dol-guldur-005"), (Cards.dungeonDoor, "attack-on-dol-guldur-006")]
    for_ [stageCode | (card, stageCode) <- stages, code == toCardCode card] $ \stageCode ->
      when (a.placement == StagingArea) do
        current <- getMainQuestCards
        unless (any (\stage -> toCardCode stage == stageCode && stage.flipped) current) $
          modifySelf a [CannotBeTraveledTo, ProgressLimit 0]
    when (code == toCardCode Cards.gateOfDolGuldurAttack && a.placement == StagingArea) $
      getRemainingPlayers >>= traverse_ (\pid -> modified a (PlayerTarget pid) [PlayerCardThreatReductionPerRoundLimit 1])
    when (code == toCardCode Cards.dungeonDoor && a.placement == StagingArea) $
      selectCharacters AnyHero AnyAlly >>= traverse_ (\target -> modified a target [HealLimitEachRound 1])
    when (code == toCardCode Cards.theShadowRoad) $ modifySelf a [ThreatModifier $ 2 * length a.underneath]
    when (code == toCardCode Cards.forestBattleground) do
      players <- getRemainingPlayers
      totals <- traverse (fmap length . select . AllyControlledBy . PlayerWithId) players
      modifySelf a [ThreatModifier $ fromMaybe 0 $ maximumMay totals]

instance HasAbilities DolGuldurLocation where
  getAbilities (DolGuldurLocation a)
    | code == toCardCode Cards.theSorcerersTower = [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]
    | code == toCardCode Cards.theShadowRoad =
        [restricted a 2 (DuringPhase QuestPhase) $ forced $ CardPlayedWindow After (CardWithType EventType) | a.placement == StagingArea]
    | code `elem` map toCardCode [Cards.forestBattleground, Cards.blackenedWoods] = [mkAbility a 3 $ forced $ ExploredWindow When a.id]
    | otherwise = []
   where code = toCardCode a

instance RunMessage DolGuldurLocation where
  runMessage msg l@(DolGuldurLocation a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> l <$ revealEncounterCard
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      placeUnderneath a [PlayerCard card | Window After (PlayedCard _ card) <- windows]
      pure l
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      if toCardCode a == toCardCode Cards.forestBattleground
        then eachPlayer $ \pid -> searchEncounterDeckAndDiscard pid a $ CardWithType EnemyType <> CardWithTrait Orc
        else discardTopOfEncounterDeck a 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      let amount = sum [fromMaybe 0 $ cdThreat $ toCardDef card | card <- cards]
      eachPlayer (`encounterRaisesThreat` amount)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> l <$ removeProgressFromMainQuest (CardIdSource cid) 3
    Discard _ (isTarget a -> True) -> release
    RemoveFromGame (isTarget a -> True) -> release
    AddToVictoryDisplay (isTarget a -> True) -> release
    ShuffleIntoEncounterDeck (isTarget a -> True) -> release
    ReturnToTopOfEncounterDeck (isTarget a -> True) -> release
    ReturnToBottomOfEncounterDeck (isTarget a -> True) -> release
    _ -> DolGuldurLocation <$> runMessage msg a
   where
    release = do
      for_ [card | PlayerCard card <- a.underneath] putInDiscard
      DolGuldurLocation <$> runMessage msg (a {locationUnderneath = []})
