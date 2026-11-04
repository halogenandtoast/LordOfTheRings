module LordOfTheRings.Quest.Cards.LostInTheDarkFliesAndSpiders (lostInTheDarkFliesAndSpiders) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck))

newtype LostInTheDarkFliesAndSpiders = LostInTheDarkFliesAndSpiders QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Lost in the Dark, stage 2 of Flies and Spiders.
lostInTheDarkFliesAndSpiders :: QuestCard LostInTheDarkFliesAndSpiders
lostInTheDarkFliesAndSpiders = quest LostInTheDarkFliesAndSpiders Cards.lostInTheDarkFliesAndSpiders

{- | When Revealed: Reveal stage 3 and create a separate staging area for the first player
using that stage. If there are no other players in the game, discard this stage and each
card in its staging area. All other players advance to stage 4. (FAQ errata)

The other players keep this stage's staging area, its active location and this stage,
which they advance from; the first player takes his cards in play and the enemies engaged
with him to his own area (The Hobbit: On the Doorstep rules insert, "Creating a Staging
Area"; see 'LordOfTheRings.Game.Base.Separation').
-}
instance RunMessage LostInTheDarkFliesAndSpiders where
  runMessage msg q@(LostInTheDarkFliesAndSpiders attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      LostInTheDarkFliesAndSpiders <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      others <- filter (/= lead) <$> getRemainingPlayers
      questDeck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
      case (questDeck, others) of
        (_ : _, []) -> do
          enemies <- select EnemyInStagingArea
          locations <- select LocationInStagingArea
          objectives <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.objectives) . (.entities))
          traverse_ discard enemies
          traverse_ discard locations
          traverse_ discard objectives
          advanceQuest
        (stage3 : _, other : _) -> do
          separatePlayers [others, [lead]]
          focusAreaOf lead
          push $ AdvanceToQuest stage3
          focusAreaOf other
          advanceQuest
          focusAreaOf lead
        _ -> pure ()
      pure q
    _ -> LostInTheDarkFliesAndSpiders <$> runMessage msg attrs
