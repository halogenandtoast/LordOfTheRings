module LordOfTheRings.Scenario.Scenarios.TheTowerOfCirithUngol (theTowerOfCirithUngol) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveFromPool), updateLog)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Id (HeroId)
import LordOfTheRings.Matcher
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (GuardRelease (ReturnToHandWhenFree), Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Trait (Trait (Item, RingBearer))

newtype TheTowerOfCirithUngol = TheTowerOfCirithUngol ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTowerOfCirithUngol :: Scenario
theTowerOfCirithUngol = Scenario $ scenario TheTowerOfCirithUngol "the-tower-of-cirith-ungol" "The Tower of Cirith Ungol"

-- | The Item boons the players set aside as the campaign card's setup has them, kept until
-- stage 3A attaches them to Shagrat.
setAsideBoons :: ScenarioAttrs -> [PlayerCard]
setAsideBoons attrs = fromMaybe [] $ parseMaybe parseJSON attrs.meta

withBoons :: [PlayerCard] -> ScenarioAttrs -> ScenarioAttrs
withBoons boons attrs = attrs {scenarioMeta = toJSON boons}

{- | The Tower of Cirith Ungol, Orcs of Mordor and Deep Shadows make up the encounter deck
(The Mountain of Fire rules insert). Shagrat, Topmost Chamber, The Two Watchers and Last of
the Company are set aside for stage 1A.

Campaign: Setup: Each player may change heroes he controls without incurring the +1 threat
penalty (see 'LordOfTheRings.Campaign.freeHeroChange'). Attach A Heavy Burden to the hero
with Last of the Company attached. Each player chooses an Item boon (excluding Andúril)
from the campaign pool and sets it aside, out of play. Remove each burden with the
following burden set icons from the encounter deck and flip this card over: HD, TRtI,
TPotGC (see 'LordOfTheRings.Saga.removedBurdenSets').
-}
instance RunMessage TheTowerOfCirithUngol where
  runMessage msg (TheTowerOfCirithUngol attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheTowerOfCirithUngol, EncounterSet.OrcsOfMordor, EncounterSet.DeepShadows]
      let aside c = toCardCode c `elem` map toCardCode [Enemies.shagrat, Locations.topmostChamber, Locations.theTwoWatchers, Objectives.lastOfTheCompany]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.passTheTwoWatchers
      laterStages <- traverse genEncounterCard [Quests.fightingInTheTower, Quests.escapeFromCirithUngol]
      putQuestIntoPlay firstStage
      -- the boons are set aside before any card is drawn or attached at setup
      when (isJust attrs.campaign) $ eachPlayer \pid -> do
        boons <- itemBoonsOf pid
        chooseOrRunOneM pid do
          questionLabeled "theTowerOfCirithUngol.setAsideBoon"
          showing (map PlayerCard boons)
          for_ boons \pc -> targeting (CardIdTarget pc.id) do
            removeFromDeck pid pc.id
            push $ ScenarioSpecific "cirithUngol.setAsideBoon" (toJSON pc)
      pure
        $ TheTowerOfCirithUngol
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    ScenarioSpecific "cirithUngol.setAsideBoon" (parseMaybe parseJSON -> Just pc) ->
      pure $ TheTowerOfCirithUngol $ withBoons (setAsideBoons attrs <> [pc]) attrs
    DoStep 9 Setup | isJust attrs.campaign -> do
      company <- lastOfTheCompanyHost
      for_ company (attachAHeavyBurden attrs)
      TheTowerOfCirithUngol <$> runMessage msg (attrs {scenarioCampaignCardFlipped = True})
    -- Forced: When the players advance to stage 3A, attach A Heavy Burden to the Ring-bearer.
    -- Attach each set aside Item boon to Shagrat as a guarded objective. When Shagrat is
    -- defeated, return each guarded boon to its owner's hand. The Ring-bearer is the hero
    -- the Ring-bearer trait is his own, not the one Last of the Company gives, which stage
    -- 3A sends to the victory display; with Shagrat out of play the boons stay set aside.
    AdvanceToQuest card | isJust attrs.campaign, toCardCode card == toCardCode Quests.escapeFromCirithUngol -> do
      company <- lastOfTheCompanyHost
      ringBearers <- filter ((/= company) . Just) <$> select (HeroWithTrait RingBearer)
      for_ (take 1 ringBearers) (attachAHeavyBurden attrs)
      shagrat <- selectOne (EnemyWithCardCode $ toCardCode Enemies.shagrat)
      case shagrat of
        Just eid -> do
          cid <- (.id) . toCard <$> getEnemy eid
          for_ (setAsideBoons attrs) (guardPlayerCard cid ReturnToHandWhenFree)
          TheTowerOfCirithUngol <$> runMessage msg (withBoons [] attrs)
        Nothing -> TheTowerOfCirithUngol <$> runMessage msg attrs
    -- Resolution: If Shagrat is in play, remove each Item boon attached to it from the
    -- campaign pool. (The players have lost those boons.)
    ResolveCampaign | Just l <- attrs.campaign -> do
      shagrat <- selectOne (EnemyWithCardCode $ toCardCode Enemies.shagrat)
      lost <- case shagrat of
        Just eid -> playerCardsGuardedBy . (.id) . toCard =<< getEnemy eid
        Nothing -> pure []
      let l' = foldl' (flip updateLog) l [RemoveFromPool (toCardCode pc) | pc <- lost]
      TheTowerOfCirithUngol <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheTowerOfCirithUngol <$> runMessage msg attrs
   where
    -- the Item boons of the player's seat in the Campaign Pool, Andúril excepted, in his deck
    itemBoonsOf pid = do
      seat <- seatOf pid
      deck <- (.deck) <$> getPlayer pid
      let pool = [p.code | p <- foldMap (.pool) attrs.campaign, isJust seat, p.seat == seat]
          isItemBoon pc =
            let def = toCardDef pc
                Name title _ = cdName def
             in toCardCode pc `elem` pool && Item `member` cdCardTraits def && title /= "Andúril"
      pure [pc | PlayerCard pc <- deck, isItemBoon pc]
    lastOfTheCompanyHost =
      getsGame
        $ headMay
        . (\os -> [hid | o <- os, toCardCode o == toCardCode Objectives.lastOfTheCompany, AttachedTo (HeroTarget hid) <- [o.placement]])
        . toList
        . (.entities.objectives)

-- | A Heavy Burden goes to the hero: the one in play, moved, or the Campaign Pool's, should
-- none be in play.
attachAHeavyBurden :: ReverseQueue m => ScenarioAttrs -> HeroId -> m ()
attachAHeavyBurden attrs hid = do
  burdens <- getsGame (filter ((== toCardCode Objectives.aHeavyBurden) . toCardCode) . toList . (.entities.objectives))
  case burdens of
    o : _ -> placeObjective (toId o) (AttachedTo $ HeroTarget hid)
    []
      | toCardCode Objectives.aHeavyBurden `elem` map (.code) (foldMap (.pool) attrs.campaign) -> do
          card <- genEncounterCard Objectives.aHeavyBurden
          putObjectiveIntoPlayAttached card (HeroTarget hid)
      | otherwise -> pure ()
