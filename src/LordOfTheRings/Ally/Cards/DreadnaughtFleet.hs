module LordOfTheRings.Ally.Cards.DreadnaughtFleet (
  eithiliant,
  eithiliantUpgraded,
  goldenWing,
  goldenWingUpgraded,
  prideOfLebennin,
  prideOfLebenninUpgraded,
  silentMist,
  silentMistUpgraded,
  swanShipOfDolAmroth,
  swanShipOfDolAmrothUpgraded,
  twilightsCall,
  twilightsCallUpgraded,
) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card hiding (ally, hero)
import LordOfTheRings.Epic.Types (EpicEffect (..), EpicTargetQuery (..))
import LordOfTheRings.Helpers.Discard (movableDiscardCards)
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound, getModifiers)
import LordOfTheRings.Helpers.Ready (readyableCards)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import {-# SOURCE #-} LordOfTheRings.Player.Runner (collectionEventChoices, playableCardsByEffect)
import LordOfTheRings.Trait (Trait (Upgraded))

newtype DreadnaughtFleet = DreadnaughtFleet AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

eithiliant
  , eithiliantUpgraded
  , goldenWing
  , goldenWingUpgraded
  , prideOfLebennin
  , prideOfLebenninUpgraded
  , silentMist
  , silentMistUpgraded
  , swanShipOfDolAmroth
  , swanShipOfDolAmrothUpgraded
  , twilightsCall
  , twilightsCallUpgraded
    :: AllyCard DreadnaughtFleet
eithiliant = ally DreadnaughtFleet Cards.eithiliant
eithiliantUpgraded = ally DreadnaughtFleet Cards.eithiliantUpgraded
goldenWing = ally DreadnaughtFleet Cards.goldenWing
goldenWingUpgraded = ally DreadnaughtFleet Cards.goldenWingUpgraded
prideOfLebennin = ally DreadnaughtFleet Cards.prideOfLebennin
prideOfLebenninUpgraded = ally DreadnaughtFleet Cards.prideOfLebenninUpgraded
silentMist = ally DreadnaughtFleet Cards.silentMist
silentMistUpgraded = ally DreadnaughtFleet Cards.silentMistUpgraded
swanShipOfDolAmroth = ally DreadnaughtFleet Cards.swanShipOfDolAmroth
swanShipOfDolAmrothUpgraded = ally DreadnaughtFleet Cards.swanShipOfDolAmrothUpgraded
twilightsCall = ally DreadnaughtFleet Cards.twilightsCall
twilightsCallUpgraded = ally DreadnaughtFleet Cards.twilightsCallUpgraded

data FleetShip = Eithiliant | GoldenWing | Pride | SilentMist | Swan | Twilight
  deriving stock Eq

kind :: AllyAttrs -> FleetShip
kind a
  | code `elem` map toCardCode [Cards.eithiliant, Cards.eithiliantUpgraded] = Eithiliant
  | code `elem` map toCardCode [Cards.goldenWing, Cards.goldenWingUpgraded] = GoldenWing
  | code `elem` map toCardCode [Cards.prideOfLebennin, Cards.prideOfLebenninUpgraded] = Pride
  | code `elem` map toCardCode [Cards.silentMist, Cards.silentMistUpgraded] = SilentMist
  | code `elem` map toCardCode [Cards.swanShipOfDolAmroth, Cards.swanShipOfDolAmrothUpgraded] = Swan
  | otherwise = Twilight
 where
  code = toCardCode a

upgraded :: AllyAttrs -> Bool
upgraded = member Upgraded . cdCardTraits . toCardDef

encounterSource :: AllyAttrs -> Source
encounterSource = CardCodeSource . toCardCode

playMatcher :: AllyAttrs -> CardMatcher
playMatcher a | kind a == SilentMist = CardWithType EventType <> CardWithPrintedCostAtLeast 3
playMatcher _ = CardMatchAny [CardWithType AllyType, CardWithType AttachmentType]

discount :: AllyAttrs -> Int
discount a | kind a == SilentMist = if upgraded a then 2 else 1
discount _ = 0

instance HasModifiersFor DreadnaughtFleet where
  getModifiersFor (DreadnaughtFleet a) = do
    shipModifiers a
    for_ (shipController a) \pid -> do
      heroes <- select $ HeroControlledBy $ PlayerWithId pid
      let bonus = if upgraded a then 2 else 1
      for_ heroes \hero -> modified a hero $ case kind a of
        Eithiliant -> [WillpowerModifier bonus]
        GoldenWing -> HitPointModifier bonus : [GainKeyword Ranged | upgraded a]
        Pride -> [DefenseModifier bonus]
        _ -> []
      case kind a of
        Pride | not (upgraded a) -> modified a (PlayerTarget pid) [StartingHandSize 5, CannotMulligan]
        SilentMist | not (upgraded a) -> modified a (PlayerTarget pid) [StartingHandSize 2]
        Twilight -> do
          played <- cardsPlayedThisRound
          when (upgraded a || not (any (\(player, _, _) -> player == pid) played))
            $ modified a (PlayerTarget pid) [NextIgnoresResourceMatch AnyCard]
        _ -> pure ()

instance HasAbilities DreadnaughtFleet where
  getAbilities (DreadnaughtFleet a) = case shipController a of
    Nothing -> []
    Just pid ->
      [mkAbility a 2 $ response $ FlippedToWindow After (toCardCode a) | upgraded a]
        <> [ mkAbility a 3 $ silent $ entersPlay a
           | not (upgraded a)
           , kind a `elem` [Eithiliant, GoldenWing, Swan]
           ]
        <> [mkAbility a 5 $ forced $ SetupHandDrawnWindow After pid | not (upgraded a), kind a == Pride]
        <> case kind a of
          SilentMist -> [restricted a 1 (PlayableCardInHand pid (playMatcher a) (discount a)) $ ActionAbility Free]
          Twilight -> [limit $ restricted a 1 (PlayableCardInHand pid (playMatcher a) 0) $ ActionAbility Free]
          Swan ->
            [ limited (GroupLimit PerRound 1)
                $ restricted a 1 (ReadyableCardExists $ not $ upgraded a)
                $ ActionAbility
                $ SpendResourcesCost Neutral (if upgraded a then 1 else 2)
            ]
          _ -> []
   where
    limit = if upgraded a then id else limited (GroupLimit PerRound 1)

instance RunMessage DreadnaughtFleet where
  runMessage msg s@(DreadnaughtFleet a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      if kind a == Swan
        then do
          candidates <- readyableCards $ not $ upgraded a
          chooseOneM pid do
            questionLabeled "dreadnaught.readyCard"
            causedBy a
            targets candidates encounterReadies
        else do
          cards <- playableCardsByEffect pid (playMatcher a) (discount a)
          chooseOneM pid do
            questionLabeled "dreadnaught.playCard"
            causedBy a
            showing cards
            targets cards \card -> do
              when (discount a > 0)
                $ createEffect
                  (encounterSource a)
                  (PlayerTarget pid)
                  [LowerNextCostOf (CardWithCode $ toCardCode card) (discount a)]
                  UntilSpent
              playFromHand pid card
      pure s
    UseCardAbility pid (isSource a -> True) 3 _ -> do
      heroes <- select $ HeroControlledBy $ PlayerWithId pid
      case kind a of
        Eithiliant -> chooseOneM pid do
          questionLabeled "dreadnaught.gainTraits"
          causedBy a
          targets [hero | hero <- heroes, length heroes > 1] \hero -> do
            chooseOneM pid do
              questionLabeled "dreadnaught.copyTraits"
              causedBy a
              targets (filter (/= hero) heroes) \other -> do
                traits <- cdCardTraits . toCardDef <$> getHero other
                createEffect (encounterSource a) hero (map GainTrait $ toList traits) UntilSpent
        GoldenWing -> do
          lowerThreatBy (encounterSource a) pid 3
          chooseOneM pid do
            questionLabeled "dreadnaught.gainRanged"
            causedBy a
            targets heroes \hero -> createEffect (encounterSource a) hero [GainKeyword Ranged] UntilSpent
        Swan -> for_ heroes \hero -> placeResources (encounterSource a) hero 1
        _ -> pure ()
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      case kind a of
        Eithiliant -> doStep 6 msg
        GoldenWing -> chooseEpicTarget a pid EpicEnemy (EpicDamage 6)
        Pride -> do
          characters <- filterM (fmap (> 0) . getDamage) =<< selectCharacters AnyHero AnyAlly
          chooseOrRunOneM pid do
            questionLabeled "dreadnaught.healCharacter"
            causedBy a
            targets characters \character -> healDamage (encounterSource a) character 2
          doStep 2 msg
        SilentMist -> doStep 2 msg
        Swan -> do
          heroes <- select AnyHero
          chooseOneM pid do
            questionLabeled "dreadnaught.resources"
            causedBy a
            targets heroes \hero -> placeResources (encounterSource a) hero 2
          doStep 1 msg
        Twilight -> drawCards (encounterSource a) pid 3
      pure s
    DoStep 1 (UseCardAbility pid (isSource a -> True) 2 _) | kind a == Swan -> do
      candidates <- readyableCards False
      chooseOneM pid do
        questionLabeled "dreadnaught.readyCard"
        causedBy a
        targets candidates encounterReadies
      pure s
    -- Eithiliant's progress is independent of choosing zero discard cards.
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 2 _) | kind a == Eithiliant -> do
      if n <= 0
        then chooseEpicTarget a pid EpicLocation (EpicProgress 6)
        else do
          locked <- elem DiscardPilesLocked <$> getModifiers GameTarget
          cards <- if locked then pure [] else movableDiscardCards . (.discard) =<< getPlayer pid
          chooseOneM pid do
            questionLabeled "dreadnaught.shuffleDiscard"
            causedBy a
            showing cards
            targets cards \card -> do
              shuffleFromDiscardIntoDeck pid card.id
              doStep (n - 1) inner
            labeled "done" $ chooseEpicTarget a pid EpicLocation (EpicProgress 6)
      pure s
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 2 _)
      | n > 0
      , kind a == Pride -> do
          cards <- getEncounterDiscard
          chooseOneM pid do
            questionLabeled "dreadnaught.addVictory"
            causedBy a
            showing (map EncounterCard cards)
            targets cards \card -> do
              addToVictoryDisplay (CardIdTarget card.id)
              doStep (n - 1) inner
            labeled "done" (pure ())
          pure s
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 2 _)
      | n > 0
      , kind a == SilentMist -> do
          locked <- elem DiscardPilesLocked <$> getModifiers GameTarget
          player <- getPlayer pid
          choices <- collectionEventChoices pid
          -- Give collection candidates stable synthetic display ids; selecting one
          -- creates its real card only after deckbuilding rules are rechecked.
          prototypes <- traverse (\def -> genPlayerCard def (Just pid)) choices
          let discarded = [c | not locked, c <- player.discard, cardMatches (CardWithType EventType) c]
          chooseOneM pid do
            questionLabeled "dreadnaught.takeEvents"
            causedBy a
            showing (discarded <> map PlayerCard prototypes)
            targets discarded \card -> do
              returnFromDiscardToHand pid card.id
              doStep (n - 1) inner
            for_ prototypes \card -> targeting (CardIdTarget card.id) do
              push $ AddCollectionCardToHand pid (toCardCode card)
              doStep (n - 1) inner
            labeled "done" (pure ())
          pure s
    UseCardAbility _ (isSource a -> True) 5 _ -> do
      doStep 5 msg
      pure s
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 5 _) | n > 0 -> do
      hand <- (.hand) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "dreadnaught.returnFive"
        causedBy a
        showing hand
        targets hand \card -> do
          shuffleFromHandIntoOwnersDeck pid card
          doStep (n - 1) inner
      pure s
    _ -> DreadnaughtFleet <$> runShip (pure ()) msg a
