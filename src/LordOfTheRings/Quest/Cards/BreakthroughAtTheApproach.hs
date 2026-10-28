module LordOfTheRings.Quest.Cards.BreakthroughAtTheApproach (breakthroughAtTheApproach) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype BreakthroughAtTheApproach = BreakthroughAtTheApproach QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

breakthroughAtTheApproach :: QuestCard BreakthroughAtTheApproach
breakthroughAtTheApproach = quest BreakthroughAtTheApproach Cards.breakthroughAtTheApproach

-- | Battle.
-- Forced: After the players quest unsuccessfully, instead of raising threat, each player
-- must choose and discard 1 character he controls.
instance HasAbilities BreakthroughAtTheApproach where
  getAbilities (BreakthroughAtTheApproach a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow When]

instance RunMessage BreakthroughAtTheApproach where
  runMessage msg q@(BreakthroughAtTheApproach attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        doesNotRaiseThreat p
        chooseAndDiscard attrs p =<< charactersControlledBy p
      pure q
    _ -> BreakthroughAtTheApproach <$> runMessage msg attrs
