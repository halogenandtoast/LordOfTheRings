module LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields (theBattleOfThePelennorFields) where

import Data.Aeson.Types (parseMaybe)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (CorsairFleetResources, HaradEnemies), PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (..), cardMatches, getLead, getPlayer, getPlayerOrder)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (AdvancedToStage))
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (harad)

newtype TheBattleOfThePelennorFields = TheBattleOfThePelennorFields ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfThePelennorFields :: Scenario
theBattleOfThePelennorFields =
  Scenario $ scenario TheBattleOfThePelennorFields "the-battle-of-the-pelennor-fields" "The Battle of the Pelennor Fields"

-- | Aragorn and what the campaign card attached to him, set aside out of play until The
-- Corsair Fleet brings him.
data SetAsideAragorn = SetAsideAragorn
  { aragornCard :: PlayerCard
  , aragornAttachments :: [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | The Battle of the Pelennor Fields' encounter set makes up the encounter deck (MEC54
rules). Minas Tirith, Grond and The Witch-king are set aside for stage 1A, with the 4
copies of Wraith on Wings, burdens with the set's icon, which are in no encounter set.

The first player takes control of the Fellowship sphere Aragorn as the game sets up (see
'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage TheBattleOfThePelennorFields where
  runMessage msg (TheBattleOfThePelennorFields attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheBattleOfThePelennorFields]
      let aside c = toCardCode c `elem` map toCardCode [Objectives.minasTirith, Locations.grond, Enemies.theWitchKing]
          (setAside', deck) = partition aside cards
      wraiths <- replicateM 4 (genEncounterCard Enemies.wraithOnWings)
      firstStage <- genEncounterCard Quests.theGarrisonOfGondor
      laterStages <- traverse genEncounterCard [Quests.retreatFromOsgiliath, Quests.theWhiteCityBesieged, Quests.rohanHasCome, Quests.fightingInTheFields]
      putQuestIntoPlay firstStage
      pure
        $ TheBattleOfThePelennorFields
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside' <> wraiths
        & questDeckL .~ laterStages
    -- Campaign: Setup: Set each Harad enemy recorded in the campaign log aside, out of play.
    -- The first player attaches Andúril and Banner of Elendil to Aragorn and sets him
    -- aside, out of play. Add The Corsair Fleet to the staging area (Gondor side faceup)
    -- and place X resources on it, where X is the number recorded in the campaign log.
    -- Remove each burden with the following burden set icons from the encounter deck (see
    -- 'LordOfTheRings.Saga.removedBurdenSets').
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      for_ (Map.toList $ Map.findWithDefault mempty HaradEnemies l.counts) \(code, n) ->
        for_ (lookupCardDef code) \def -> replicateM_ n (setAside =<< genEncounterCard def)
      lead <- getLead
      aragorns <- getsGame (filter ((== toCardCode Heroes.aragornTheFlameOfTheWest) . toCardCode) . toList . (.entities.heroes))
      meta <- for (headMay [(h, pc) | h <- aragorns, PlayerCard pc <- [toCard h]]) \(h, pc) -> do
        attachments <- traverse (takeAttachment lead) [Attachments.anduril, Attachments.bannerOfElendil]
        setAsideHero h.id
        pure $ SetAsideAragorn pc attachments
      fleet <- genEncounterCard Objectives.theCorsairFleetGondor
      addToStaging fleet
      let x = sum $ Map.findWithDefault mempty CorsairFleetResources l.counts
      placeResources ScenarioSource (CardIdTarget fleet.id) x
      TheBattleOfThePelennorFields <$> runMessage msg (attrs & metaL .~ maybe Null toJSON meta)
    -- The Corsair Fleet's Forced effect: the first player takes control of Aragorn, with
    -- what the campaign card attached to him
    ScenarioSpecific "pelennorFields.aragornJoins" _ | Just (aside :: SetAsideAragorn) <- parseMaybe parseJSON attrs.meta -> do
      lead <- getLead
      hid <- getRandom
      push $ PutHeroIntoPlayAs hid lead aside.aragornCard
      for_ aside.aragornAttachments \card -> attachOutOfPlay (fromMaybe lead card.owner) card (HeroTarget hid)
      pure $ TheBattleOfThePelennorFields $ attrs & metaL .~ Null
    -- Forced: When the players advance to stage 5B, add each set aside Harad enemy to the
    -- staging area.
    CheckWindows ws | isJust attrs.campaign, Window After AdvancedToStage `elem` ws -> do
      stages <- getsGame (map toCardCode . toList . (.entities.quests))
      when (toCardCode Quests.fightingInTheFields `elem` stages) $ addSetAsideToStaging harad
      TheBattleOfThePelennorFields <$> runMessage msg attrs
    -- Resolution: Add 1 copy, per player, of Wraith on Wings to the campaign pool. The
    -- players have earned those burdens. If Esquire of Gondor, or Esquire of Rohan, is
    -- attached to a hero, that hero's controller has earned that boon.
    ResolveCampaign | Just l <- attrs.campaign -> do
      order <- getsGame (.playerOrder)
      heroes <- getsGame (toList . (.entities.heroes))
      attached <- getsGame (toList . (.entities.attachments))
      let wraiths = [PoolCard (toCardCode Enemies.wraithOnWings) Nothing Nothing | _ <- order]
          esquires =
            [ PoolCard (toCardCode a) (elemIndex h.owner order) (Just $ toCardCode h)
            | a <- attached
            , toCardCode a `elem` map toCardCode [Attachments.esquireOfGondor, Attachments.esquireOfRohan]
            , h <- heroes
            , a.placement == AttachedTo (HeroTarget h.id)
            ]
          l' = foldl' (flip addToPool) l (wraiths <> esquires)
      TheBattleOfThePelennorFields <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheBattleOfThePelennorFields <$> runMessage msg attrs
   where
    -- the copy in a player's hand or deck, the first player's first, or else one from
    -- outside the game
    takeAttachment lead def = do
      players <- getPlayerOrder
      held <- for (lead : filter (/= lead) players) \p -> do
        player <- getPlayer p
        pure $ [(p, pc, True) | PlayerCard pc <- player.hand, cardMatches (CardWithCode $ toCardCode def) (PlayerCard pc)]
          <> [(p, pc, False) | PlayerCard pc <- player.deck, cardMatches (CardWithCode $ toCardCode def) (PlayerCard pc)]
      case concat held of
        (p, pc, inHand) : _ -> do
          if inHand then removeFromHand p pc else removeFromDeck p pc.id
          pure pc
        [] -> genPlayerCard def (Just lead)
