module LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony (murderAtThePrancingPony) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Matcher (getLead, getEnemy, getRemainingPlayers, select, selectAny, cardIs, CardMatcher (..), EnemyMatcher (..), PlayerMatcher (..))
import LordOfTheRings.Ability
import LordOfTheRings.Trait (Trait (Suspect))
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Source
import LordOfTheRings.Window

newtype MurderAtThePrancingPony = MurderAtThePrancingPony ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

murderAtThePrancingPony :: Scenario
murderAtThePrancingPony = Scenario $ scenario MurderAtThePrancingPony "murder-at-the-prancing-pony" "Murder at the Prancing Pony"

withMeta :: Meta -> ScenarioAttrs -> ScenarioAttrs
withMeta meta a = a & metaL .~ toJSON meta

instance HasModifiersFor MurderAtThePrancingPony where
  getModifiersFor (MurderAtThePrancingPony a) = do
    when (notNull $ (getMeta a).investigationDeck) $ modified a GameTarget [InvestigationDeckHasCards]
    enemies <- select $ EnemyWithTrait Suspect
    players <- getRemainingPlayers
    for_ a.victoryDisplay $ \card -> do
      -- These locations keep encounter-card effects in victory. A bare card-id
      -- source is otherwise treated as a player event after its entity leaves play.
      let code = toCardCode card
      for_ enemies $ \eid -> do
        when (code == toCardCode Locations.billFernysHouse) do
          modified (CardCodeSource code) eid [ImmuneToPlayerCardEffects]
          suspectEnemy <- getEnemy eid
          for_ (toAttrs suspectEnemy).shadowCards $ \shadow -> modified (CardCodeSource code) (CardIdTarget shadow.id) [ImmuneToPlayerCardEffects]
        when (code == toCardCode Locations.staddleHidingHole) $
          for_ players $ \pid -> unlessM (selectAny $ EnemyWithId eid <> EnemyEngagedWith (PlayerWithId pid)) $
            modified (CardCodeSource code) eid [CannotBeAttackedByCharactersOf pid]
        when (code == toCardCode Locations.archetTradingPost) $ modified (CardCodeSource code) eid [DamageLimitEachRound 5]
        when (code == toCardCode Locations.chetwoodCampsite) $ modified (CardCodeSource code) eid [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]

instance HasAbilities MurderAtThePrancingPony where
  getAbilities (MurderAtThePrancingPony a) =
    [restricted a 1 (CardInVictoryDisplay $ cardIs Locations.combeStorehouse) $ forced $ MatchingEnemyEngagedWindow After (EnemyWithTrait Suspect)]

showCards :: ReverseQueue m => PlayerId -> Source -> [EncounterCard] -> m ()
showCards pid source cards = if null cards
  then push $ InvestigationLookedAt pid source []
  else chooseOneM pid do
    questionLabeled "prancingPony.look"
    for_ (sourceTarget source) causedBy
    showing $ map EncounterCard cards
    -- Other players may see the choice's messages after the search cards are
    -- hidden. Keep the looked-at identities in private scenario state instead.
    labeled "continue" $ push $ DoStep 1 $ InvestigationLookedAt pid source []

instance RunMessage MurderAtThePrancingPony where
  runMessage msg s@(MurderAtThePrancingPony a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.MurderAtThePrancingPony]
      putQuestIntoPlay =<< genEncounterCard Quests.investigatingTheCrime
      later <- traverse genEncounterCard [Quests.runningOutOfTime, Quests.apprehendingTheKiller]
      pure $ MurderAtThePrancingPony $ withMeta emptyMeta $ a & encounterDeckL .~ deck & questDeckL .~ later
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [(eid, pid) | Window After (EnemyEngaged eid pid) <- windows] $ \(eid, pid) -> immediateAttack pid eid
      pure s
    StageInvestigationCard card | card `elem` meta.investigationDeck -> do
      addToStaging card
      pure $ MurderAtThePrancingPony $ withMeta (meta {investigationDeck = filter (/= card) meta.investigationDeck}) a
    ShuffleIntoInvestigationDeck card -> do
      removeFromEncounterDiscard card
      shuffled <- shuffleM $ card : filter (/= card) meta.investigationDeck
      pure $ MurderAtThePrancingPony $ withMeta (meta {investigationDeck = shuffled}) a
    ScenarioSpecific "pony.loseClue" _ ->
      if null meta.investigationDeck then pure s else do
        card <- uniform meta.investigationDeck
        pure $ MurderAtThePrancingPony $ withMeta (meta {investigationDeck = filter (/= card) meta.investigationDeck, removed = card : meta.removed}) a
    ScenarioSpecific "pony.setup" _ -> do
      let deck = a.encounterDeck
          matching defs = filter ((`elem` map toCardCode defs) . toCardCode)
          allSuspects = matching suspects deck
          allHideouts = matching hideouts deck
      killer <- uniform allSuspects
      hideout <- uniform allHideouts
      let solutionCards = [killer, hideout]
          investigation = filter (`notElem` solutionCards) (allSuspects <> allHideouts)
            <> matching [Treacheries.takenBySurprise] deck
          pony = matching [Locations.thePrancingPonyMurder] deck
          manor = take 1 $ matching [Locations.ramshackleManor] deck
          taken = solutionCards <> investigation <> pony <> manor
      clues <- shuffleM investigation
      rest <- shuffleM $ filter (`notElem` taken) deck
      traverse_ (push . PutIntoPlayAsActiveLocation) pony
      traverse_ addToStaging manor
      pure $ MurderAtThePrancingPony $ withMeta (emptyMeta {investigationDeck = clues, hidden = solutionCards}) $ a & encounterDeckL .~ rest
    ResolveInvestigate _ lid -> do
      ignored <- elem IgnoreInvestigate <$> getModifiers GameTarget
      value <- investigateValue lid
      unless (ignored || value <= 0) do
        push $ CheckWindows [mkWhen $ Investigating lid]
        doStep 1 msg
      pure s
    DoStep 1 inner@(ResolveInvestigate pid lid) -> do
      value <- investigateValue lid
      mods <- getModifiers lid
      effects <- getsGame gameEffects
      for_ [e | e <- effects, effectTarget e == toTarget lid, any (\case AdditionalInvestigateCards _ -> True; _ -> False) (effectModifiers e)] $ push . EndEffect
      shuffled <- shuffleM meta.investigationDeck
      lookAtInvestigation pid (LocationSource lid) (value + sum [n | AdditionalInvestigateCards n <- mods])
      doStep 2 inner
      pure $ MurderAtThePrancingPony $ withMeta (meta {investigationDeck = shuffled}) a
    DoStep 2 (ResolveInvestigate pid lid) -> do
      push $ CheckWindows [mkAfter $ Investigated lid $ Map.findWithDefault [] pid meta.lastLook]
      pure s
    LookAtInvestigationDeck pid source n -> do
      let cards = take (max 0 n) meta.investigationDeck
      showCards pid source cards
      pure $ MurderAtThePrancingPony $ withMeta (meta {lastLook = Map.insert pid cards meta.lastLook}) a
    LookAtBottomOfInvestigationDeck pid source -> do
      let cards = take 1 $ reverse meta.investigationDeck
      showCards pid source cards
      pure $ MurderAtThePrancingPony $ withMeta (meta {lastLook = Map.insert pid cards meta.lastLook}) a
    DoStep 1 (InvestigationLookedAt pid source _) -> do
      push $ InvestigationLookedAt pid source $ Map.findWithDefault [] pid meta.lastLook
      pure s
    InvestigationLookedAt pid _ cards -> do
      for_ [c | c <- cards, toCardCode c == toCardCode Treacheries.takenBySurprise] $ \card ->
        push $ ScenarioSpecific "pony.surprise" $ toJSON card.id
      pure $ MurderAtThePrancingPony $ withMeta
        (meta {known = Map.insertWith (\new old -> ordNub $ old <> new) pid (map toCardCode cards) meta.known, lastLook = Map.insert pid cards meta.lastLook}) a
    ScenarioSpecific "pony.surprise" _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      pure s
    DoStep 2 inner@(ScenarioSpecific "pony.surprise" (parseMaybe parseJSON -> Just cid)) -> do
      discardEncounterCardsUntilFor (CardIdSource cid) (CardIdTarget cid) (CardWithType EnemyType)
      doStep 1 inner
      pure s
    DiscardedTopOfEncounterDeck (CardIdSource cid) _ cards
      | any (\c -> c.id == cid && toCardCode c == toCardCode Treacheries.takenBySurprise) meta.investigationDeck -> do
        lead <- getLead
        for_ (take 1 $ filter ((== EnemyType) . cdCardType . toCardDef) cards) $ \card -> do
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged lead card
        pure s
    DoStep 1 (ScenarioSpecific "pony.surprise" (parseMaybe parseJSON -> Just cid)) -> do
      let (gone, rest) = partition ((== cid) . (.id)) meta.investigationDeck
      pure $ MurderAtThePrancingPony $ withMeta (meta {investigationDeck = rest, removed = meta.removed <> gone}) a
    MakeAccusation pid Nothing _ -> do
      chooseOneM pid do
        questionLabeled "prancingPony.nameSuspect"
        for_ suspects $ \def -> labeledWith "prancingPony.nameCard" (cardVar "card" def) $
          push $ MakeAccusation pid (Just $ toCardCode def) Nothing
      pure s
    MakeAccusation pid suspect Nothing -> do
      chooseOneM pid do
        questionLabeled "prancingPony.nameHideout"
        for_ hideouts $ \def -> labeledWith "prancingPony.nameCard" (cardVar "card" def) $
          push $ MakeAccusation pid suspect (Just $ toCardCode def)
      pure s
    MakeAccusation _ (Just suspect) (Just hideout) -> do
      let correct = all (`elem` map toCardCode meta.hidden) [suspect, hideout]
          additional = [c | c <- meta.investigationDeck, toCardCode c `elem` [suspect, hideout], not correct]
      traverse_ addToStaging $ meta.hidden <> additional
      unless correct $ eachPlayer $ \pid -> encounterRaisesThreat pid 3
      push $ AccusationMade correct
      pure $ MurderAtThePrancingPony $ withMeta (meta {hidden = [], investigationDeck = filter (`notElem` additional) meta.investigationDeck, accusationCorrect = Just correct}) a
    _ -> MurderAtThePrancingPony <$> runMessage msg a
   where
    meta = getMeta a
