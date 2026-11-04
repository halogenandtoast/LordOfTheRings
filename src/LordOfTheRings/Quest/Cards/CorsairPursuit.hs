module LordOfTheRings.Quest.Cards.CorsairPursuit (corsairPursuit) where

import LordOfTheRings.Card (CardType (EnemyType), cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Ship))

newtype CorsairPursuit = CorsairPursuit QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairPursuit :: QuestCard CorsairPursuit
corsairPursuit = quest CorsairPursuit Cards.corsairPursuit

-- | When Revealed: Search the encounter deck and discard pile for a Ship enemy with the
-- highest threat and add it to the staging area (2 Ship enemies with the highest threat
-- instead if there are 3 or more players in the game). Shuffle the encounter deck.
-- Sailing. You cannot place progress on Corsair Pursuit unless you are on-course. If
-- there are no Ship enemies in play, you have defeated your pursuers, and win the game. If
-- this stage has 10 or more progress on it, you have lost your pursuers, and win the game.
instance RunMessage CorsairPursuit where
  runMessage msg q@(CorsairPursuit attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      CorsairPursuit <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      doStep 1 msg
      when (players >= 3) $ doStep 1 msg
      checkStage attrs.id
      pure q
    -- each search finds the highest threat among the Ship enemies still to be found
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let ships = filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Ship) (scenario.encounterDeck <> scenario.encounterDiscard)
          threatOf = fromMaybe 0 . cdThreat . toCardDef
          highest = maximumMay (map threatOf ships)
      lead <- getLead
      for_ highest \h ->
        searchEncounterDeckAndDiscard lead attrs $ CardMatchAny [cardIs (toCardDef c) | c <- ships, threatOf c == h]
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      onCourse <- isOnCourse
      if onCourse
        then do
          let attrs' = attrs & tokensL %~ addTokens Progress n
          when (attrs'.progress >= 10) winGame
          pure $ CorsairPursuit attrs'
        else pure q
    CheckStage qid | qid == attrs.id && attrs.flipped -> do
      none <- not <$> selectAny shipEnemy
      when none winGame
      pure q
    _ | attrs.flipped, shipLeaves -> do
      checkStage attrs.id
      CorsairPursuit <$> runMessage msg attrs
    _ -> CorsairPursuit <$> runMessage msg attrs
   where
    -- a Ship enemy leaving play may leave none behind
    shipLeaves = case msg of
      Destroy _ (EnemyTarget _) -> True
      Discard (EnemyTarget _) -> True
      AddToVictoryDisplay (EnemyTarget _) -> True
      ShuffleIntoEncounterDeck (EnemyTarget _) -> True
      ReturnToTopOfEncounterDeck (EnemyTarget _) -> True
      ReturnToBottomOfEncounterDeck (EnemyTarget _) -> True
      RemoveFromGame (EnemyTarget _) -> True
      _ -> False
