module LordOfTheRings.Quest.Cards.TrulyLost (trulyLost) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (LocationType), PlayerCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.Types (Location)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (advancingFromStage, objectiveLocationMatcher)

newtype TrulyLost = TrulyLost QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 3C of Lost in Mirkwood. No quest points.
trulyLost :: QuestCard TrulyLost
trulyLost = quest TrulyLost Cards.trulyLost

playerCardsUnder :: Location -> [PlayerCard]
playerCardsUnder l = [pc | PlayerCard pc <- (toAttrs l).underneath]

-- | Each location gets +1 quest point for each facedown player card under it.
instance HasModifiersFor TrulyLost where
  getModifiersFor (TrulyLost a) = when a.flipped do
    locations <- getsGame (toList . (.entities.locations))
    for_ locations \l -> do
      let n = length (playerCardsUnder l)
      when (n > 0) $ modified a l [QuestPointModifier n]

-- | Forced: When a location with a facedown player card under it is explored, return each
-- facedown player card under it to its owner's hand.
instance HasAbilities TrulyLost where
  getAbilities (TrulyLost a) = [mkAbility a 1 $ silent $ AnyExploredWindow When | a.flipped]

{- | When Revealed: Each player discards cards from the top of the encounter deck until he
discards a non-objective location and adds it to the staging area. Each player places his
hand of cards facedown under a different location in the staging area: in turn order,
each choosing among those not yet chosen; with no location left, a player keeps his hand.
When the players advance from this stage, discard each facedown player card under a
location.
-}
instance RunMessage TrulyLost where
  runMessage msg q@(TrulyLost attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TrulyLost <$> runMessage msg attrs
    -- one player after another, each discarding from the deck as the one before left it
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        p : _ -> do
          discardEncounterCardsUntilFor attrs (PlayerTarget p) nonObjectiveLocation
          doStep (n + 1) (WhenRevealed $ toTarget attrs)
        [] -> do
          locations <- select LocationInStagingArea
          placeHands players locations
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget _) cards -> do
      for_ (find (cardMatches nonObjectiveLocation) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [lid | Window _ (Explored lid) <- ws] \lid -> do
        l <- getLocation lid
        traverse_ putInHand (playerCardsUnder l)
      pure q
    _ | advancingFromStage msg -> do
      locations <- getsGame (toList . (.entities.locations))
      for_ locations \l -> traverse_ putInDiscard (playerCardsUnder l)
      TrulyLost <$> runMessage msg attrs
    _ -> TrulyLost <$> runMessage msg attrs
   where
    nonObjectiveLocation = CardWithType LocationType <> NotCard objectiveLocationMatcher
    placeHands :: ReverseQueue m => [PlayerId] -> [LocationId] -> m ()
    placeHands (p : ps) available@(_ : _) = do
      hand <- (.hand) <$> getPlayer p
      chooseOrRunOneM p do
        questionLabeled "trulyLost.location"
        causedBy attrs
        targets available \l -> do
          placeUnderneath l hand
          placeHands ps (filter (/= l) available)
    placeHands _ _ = pure ()
