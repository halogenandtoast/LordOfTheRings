module LordOfTheRings.Quest.Cards.TheShadowOfMordor (theShadowOfMordor) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DangerInDorwinion.Helpers (addRandomSetAside, questResources)
import LordOfTheRings.Trait (Trait (Plot))

newtype TheShadowOfMordor = TheShadowOfMordor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theShadowOfMordor :: QuestCard TheShadowOfMordor
theShadowOfMordor = quest TheShadowOfMordor Cards.theShadowOfMordor

-- | X is 3 more than the number of players.
required :: ReverseQueue m => m Int
required = (+ 3) . length <$> getRemainingPlayers

-- | This stage cannot be defeated unless there are at least X resources here.
instance HasModifiersFor TheShadowOfMordor where
  getModifiersFor (TheShadowOfMordor a) = when a.flipped do
    x <- (+ 3) . length <$> getRemainingPlayers
    when (questResources a < x) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the active location leaves play as an explored location, reveal the top
-- card of the encounter deck. Then, place 1 resource here.
instance HasAbilities TheShadowOfMordor where
  getAbilities (TheShadowOfMordor a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After | a.flipped]

{- | When Revealed: Add 1 random set aside Plot objective to the staging area. Reveal 1
encounter card per player.
Side B has 0 quest points: with no progress to wait for, it is defeated as soon as it has
X resources, those Evidence of the Cult places here too.
-}
instance RunMessage TheShadowOfMordor where
  runMessage msg q@(TheShadowOfMordor attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheShadowOfMordor <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addRandomSetAside Plot
      revealEncounterCards . length =<< getRemainingPlayers
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      placeResources attrs attrs 1
      pure q
    CheckStage qid | qid == attrs.id -> do
      x <- required
      when (attrs.flipped && questResources attrs >= x) $ completeQuest attrs.id
      pure q
    _ -> TheShadowOfMordor <$> runMessage msg attrs
