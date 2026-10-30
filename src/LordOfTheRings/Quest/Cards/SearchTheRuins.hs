module LordOfTheRings.Quest.Cards.SearchTheRuins (searchTheRuins) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SearchTheRuins = SearchTheRuins QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
searchTheRuins :: QuestCard SearchTheRuins
searchTheRuins = quest SearchTheRuins Cards.searchTheRuins

-- | Forced: At the end of the refresh phase, raise each player's threat by 2.
instance HasAbilities SearchTheRuins where
  getAbilities (SearchTheRuins a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

-- | Response: After Search the Ruins is defeated, reduce each player's threat by 3. The
-- players gain by it, so it is taken.
instance RunMessage SearchTheRuins where
  runMessage msg q@(SearchTheRuins attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid 2
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> lowerThreatBy attrs pid 3
      SearchTheRuins <$> runMessage msg attrs
    _ -> SearchTheRuins <$> runMessage msg attrs
