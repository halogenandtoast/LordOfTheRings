module LordOfTheRings.Quest.Cards.AcrossTheMarshes (acrossTheMarshes) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (firstPlayerControlsSmeagol, locationsInVictoryDisplay)

-- | Stage 2: its side B is Across the Dead Marshes.
newtype AcrossTheMarshes = AcrossTheMarshes QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheMarshes :: QuestCard AcrossTheMarshes
acrossTheMarshes = quest AcrossTheMarshes Cards.acrossTheMarshes

-- | Progress cannot be placed on locations in the staging area.
instance HasModifiersFor AcrossTheMarshes where
  getModifiersFor (AcrossTheMarshes a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Forced: At the end of the staging step, if the number of locations in the staging area
-- is less than the number of players in the game, reveal the top card of the encounter
-- deck.
instance HasAbilities AcrossTheMarshes where
  getAbilities (AcrossTheMarshes a) = [mkAbility a 1 $ forced StagingStepEndsWindow | a.flipped]

fewerLocationsThanPlayers :: HasGame m => m Bool
fewerLocationsThanPlayers = do
  locations <- length <$> select LocationInStagingArea
  players <- length <$> getRemainingPlayers
  pure $ locations < players

-- | The players cannot advance unless the first player controls Sméagol and there are at
-- least 4 locations in the victory display.
canAdvance :: HasGame m => m Bool
canAdvance = (&&) <$> firstPlayerControlsSmeagol <*> ((>= 4) . length <$> locationsInVictoryDisplay)

-- | When Revealed: If the number of locations in the staging area is less than the number
-- of players in the game, discard cards from the encounter deck until a location is
-- discarded. Then, add the discarded location to the staging area.
instance RunMessage AcrossTheMarshes where
  runMessage msg q@(AcrossTheMarshes attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AcrossTheMarshes <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      fewer <- fewerLocationsThanPlayers
      when fewer $ discardEncounterCardsUntil attrs (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      fewer <- fewerLocationsThanPlayers
      when fewer revealEncounterCard
      pure q
    -- Sméagol may come under the first player's control as Gollum turns over or the token
    -- passes
    Flip _ -> recheck
    SetFirstPlayer _ -> recheck
    _ -> AcrossTheMarshes <$> runConditionalStage canAdvance msg attrs
   where
    recheck = do
      push $ CheckStage attrs.id
      AcrossTheMarshes <$> runConditionalStage canAdvance msg attrs
