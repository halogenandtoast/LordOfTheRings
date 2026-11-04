{- | What the Angmar Awakened campaign's cards share: Iârion's Pendant on Iârion, Protect
the Innocent and Arnor Ravaged, Amarthiúl's Courage, Thaurdir's burdens, and the boons and
burdens each campaign card adds to the pool.
-}
module LordOfTheRings.Scenario.Scenarios.AngmarAwakened (
  earnAtSetup,
  iarionsPendantOnIarion,
  protectTheInnocentSetup,
  recordProtectTheInnocent,
  angmarSetup,
  thaurdirsBurdens,
  thaurdirsBurdensAtSetup,
  attachPermanentBoons,
  seatOf,
  chooseBoonForHand,
) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesHeroes, entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (QuestAttrs (questTokens))
import LordOfTheRings.Card
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign, scenarioSetAside))
import LordOfTheRings.Enemy.Types (enemyCardId)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Token

inPool :: CardDef -> CampaignLog -> Bool
inPool def l = toCardCode def `elem` map (.code) l.pool

-- | The encounter cards a campaign card adds to the Campaign Pool and shuffles into the
-- encounter deck, in the log by setup step 2, which shuffles the pool's encounter cards in.
earnAtSetup :: [CardDef] -> ScenarioAttrs -> ScenarioAttrs
earnAtSetup defs attrs = attrs {scenarioCampaign = (\l -> foldl' earn l defs) <$> attrs.campaign}
 where
  earn l def = if inPool def l then l else addToPool (PoolCard (toCardCode def) Nothing Nothing) l

-- | The player's seat, as the campaign log keeps a player card earned.
seatOf :: HasGame m => PlayerId -> m (Maybe Int)
seatOf pid = elemIndex pid <$> getsGame (.playerOrder)

-- | Setup: Attach Iârion's Pendant to Iârion, under the control of the player who has him.
iarionsPendantOnIarion :: ReverseQueue m => m ()
iarionsPendantOnIarion = do
  lead <- getLead
  iarions <- traverse getAlly =<< select (AllyWithCardCode $ toCardCode Objectives.iarion)
  for_ iarions \i -> do
    let controller = fromMaybe lead (placementController i.placement)
    card <- genPlayerCard Attachments.iarionsPendant (Just controller)
    attachOutOfPlay controller card i.id

{- | Setup: Add the Protect the Innocent side quest to the staging area and place damage on
it equal to the amount of damage noted for it in the Campaign Log. Intruders in Chetwood
offers more: starting with the first player, each player may place 4 damage on Protect the
Innocent to add 1 resource to each of their heroes' resource pools.
-}
protectTheInnocentSetup :: (ReverseQueue m, Sourceable source) => source -> CampaignLog -> Bool -> m ()
protectTheInnocentSetup source l offerResources = do
  card <- genEncounterCard Quests.protectTheInnocent
  addToStaging card
  let noted = recordedCount ProtectTheInnocentDamage (toCardCode Quests.protectTheInnocent) l
  when (noted > 0) $ placeTokens source (CardIdTarget card.id) Damage noted
  when offerResources $ eachPlayer \p -> do
    heroes <- select (HeroControlledBy $ PlayerWithId p)
    chooseOneM p do
      questionLabeled "protectTheInnocent.resources"
      labeled "protectTheInnocent.placeDamage" do
        placeTokens source (CardIdTarget card.id) Damage 4
        for_ heroes \h -> placeResources source h 1
      labeled "pass" $ pure ()

-- | Resolution: Note the amount of damage on Protect the Innocent in the Campaign Log
-- (replacing the previous number).
recordProtectTheInnocent :: HasGame m => CampaignLog -> m CampaignLog
recordProtectTheInnocent l = do
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  let code = toCardCode Quests.protectTheInnocent
  pure $ case [countTokens Damage (questTokens $ toAttrs q) | q <- quests, toCardCode q == code] of
    n : _ -> recordCount ProtectTheInnocentDamage code n l
    [] -> l

{- | What every scenario after Deadmen's Dike sets up from the pool: Arnor Ravaged in the
staging area, with the damage the log marks for it; Amarthiúl's Courage on Amarthiúl when
he is an objective ally (on the hero the scenario runner attaches it); and, at the first
setup after Deadmen's Dike, its resolution's choice of Iârion's Pendant or Amarthiúl's
Courage, which the first player makes for the group.
-}
angmarSetup :: ReverseQueue m => ScenarioAttrs -> m ()
angmarSetup attrs = for_ attrs.campaign \l -> do
  when (inPool Objectives.arnorRavaged l) do
    card <- genEncounterCard Objectives.arnorRavaged
    addToStaging card
    let noted = recordedCount ProtectTheInnocentDamage (toCardCode Quests.protectTheInnocent) l
    when (noted > 0) $ placeTokens attrs (CardIdTarget card.id) Damage noted
  when (inPool Attachments.amarthiulsCourage l) courageOnAmarthiul
  -- The Treachery of Rhudaur's resolution: "For each Clue a player controls, that player
  -- may add the book with the same name to the Campaign Pool and their deck."
  let books = Map.toList $ Map.findWithDefault mempty BooksEarned l.counts
  unless (null books) do
    updateCampaignLog (Unrecord BooksEarned)
    order <- getsGame (.playerOrder)
    for_ books \(code, seat) -> for_ (atMay order seat) \p -> do
      card <- genPlayerCard (toCardDef code) (Just p)
      chooseOneM p do
        questionLabeled "campaign.addBook"
        showing [PlayerCard card]
        labeled "campaign.addBookToDeck" $ updateCampaignLog $ AddToPool $ PoolCard code (Just seat) Nothing
        labeled "pass" $ pure ()
  when (Map.member ChoosePendantOrCourage l.notes) do
    lead <- getLead
    updateCampaignLog (Unrecord ChoosePendantOrCourage)
    chooseOneM lead do
      questionLabeled "campaign.pendantOrCourage"
      labeled "campaign.iarionsPendant" do
        choosePlayerM lead lead \p -> do
          card <- genPlayerCard Attachments.iarionsPendant (Just p)
          seat <- seatOf p
          updateCampaignLog $ AddToPool $ PoolCard (toCardCode card) seat Nothing
          shuffleIntoDeck p card
      labeled "campaign.amarthiulsCourage" do
        updateCampaignLog $ AddToPool $ PoolCard (toCardCode Attachments.amarthiulsCourage) Nothing (Just $ toCardCode Heroes.amarthiul)
        courageOnAmarthiul

-- | Amarthiúl's Courage, on the objective ally Amarthiúl, under his controller's control.
courageOnAmarthiul :: ReverseQueue m => m ()
courageOnAmarthiul = do
  lead <- getLead
  amarthiuls <- traverse getAlly =<< select (AllyWithTitle "Amarthiúl")
  for_ (take 1 amarthiuls) \a -> do
    let controller = fromMaybe lead (placementController a.placement)
    card <- genPlayerCard Attachments.amarthiulsCourage (Just controller)
    attachOutOfPlay controller card a.id

thaurdirCodes :: [CardCode]
thaurdirCodes = map toCardCode [Enemies.thaurdirDeadmensDike, Enemies.thaurdirTheTreacheryOfRhudaur, Enemies.thaurdirCaptain, Enemies.thaurdirChampion]

thaurdirBurdenDefs :: [CardDef]
thaurdirBurdenDefs = [Objectives.thaurdirsLegacy, Objectives.thaurdirsMajesty, Objectives.thaurdirsSpite]

{- | Each of Thaurdir's burdens in the pool: "Setup: Attach to Thaurdir (even if he is out
of play). Otherwise, remove this card from the game." Out of play, it is attached to his
card, wherever it lies; its Forced effect is live either way.
-}
thaurdirsBurdensAtSetup :: ReverseQueue m => ScenarioAttrs -> m ()
thaurdirsBurdensAtSetup attrs = for_ attrs.campaign \l -> do
  inPlay <- getsGame (filter ((`elem` thaurdirCodes) . toCardCode) . toList . (.entities.enemies))
  let outOfPlay = [c | c <- scenarioSetAside attrs <> attrs.encounterDeck <> attrs.encounterDiscard, toCardCode c `elem` thaurdirCodes]
      host = case inPlay of
        e : _ -> Just (EnemyTarget $ toId e)
        [] -> CardIdTarget . (.id) <$> headMay outOfPlay
  for_ host \h -> for_ (filter (`inPool` l) thaurdirBurdenDefs) \def ->
    (`putObjectiveIntoPlayAttached` h) =<< genEncounterCard def

{- | Thaurdir's burdens follow him as he enters play, from his card to him. At Deadmen's
Dike, with none earned yet: Forced: After Thaurdir enters play, randomly select one of the
following burdens and attach it to Thaurdir: Thaurdir's Legacy, Thaurdir's Majesty, or
Thaurdir's Spite. Add that card to the Campaign Pool. The scenario checks as each window
opens.
-}
thaurdirsBurdens :: ReverseQueue m => ScenarioAttrs -> Bool -> m ()
thaurdirsBurdens attrs firstTime = for_ attrs.campaign \l -> do
  thaurdirs <- getsGame (filter ((`elem` thaurdirCodes) . toCardCode) . toList . (.entities.enemies))
  objectives <- getsGame (toList . entitiesObjectives . gameEntities)
  for_ thaurdirs \t ->
    for_ [o | o <- objectives, o.placement == AttachedTo (CardIdTarget (enemyCardId $ toAttrs t))] \o ->
      attachObjective (toId o) (EnemyTarget $ toId t)
  for_ (take 1 thaurdirs) \thaurdir ->
    when (firstTime && not (any (`inPool` l) thaurdirBurdenDefs)) do
      def <- uniform (Objectives.thaurdirsLegacy :| [Objectives.thaurdirsMajesty, Objectives.thaurdirsSpite])
      (`putObjectiveIntoPlayAttached` EnemyTarget (toId thaurdir)) =<< genEncounterCard def
      updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) Nothing Nothing

{- | Escape from Mount Gram's campaign card: Forced: After a hero enters play, attach to
that hero each set-aside permanent boon attachment assigned to that character in the
Campaign Log. The copies set aside as the hero was captured went with it; new ones come in.
-}
attachPermanentBoons :: ReverseQueue m => ScenarioAttrs -> CardCode -> m ()
attachPermanentBoons attrs code = for_ attrs.campaign \l -> do
  heroes <- getsGame (filter ((== code) . toCardCode) . toList . entitiesHeroes . gameEntities)
  for_ heroes \h -> for_ [p | p <- l.pool, p.attachedTo == Just code] \p -> do
    let def = toCardDef p.code
    if isPlayerCardDef def
      then (\card -> attachOutOfPlay h.owner card h.id) =<< genPlayerCard def (Just h.owner)
      else (`putObjectiveIntoPlayAttached` h.id) =<< genEncounterCard def

{- | Each player in turn order may select one of the boons and add it to their hand, and
the Campaign Pool, for their deck. A boon one player selects is gone for those after.
-}
chooseBoonForHand :: ReverseQueue m => [CardDef] -> m ()
chooseBoonForHand defs = eachPlayerMayTakeOneOf "campaign.chooseBoon" Nothing defs \p boon -> do
  seat <- seatOf p
  putInHand boon
  updateCampaignLog $ AddToPool $ PoolCard (toCardCode boon) seat Nothing
