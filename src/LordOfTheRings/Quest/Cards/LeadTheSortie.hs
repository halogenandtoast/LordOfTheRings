module LordOfTheRings.Quest.Cards.LeadTheSortie (leadTheSortie) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (EpicEffect (EpicDiscardEnemy), EpicTargetQuery (EpicOtherNonUniqueNoVictoryEnemy))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype LeadTheSortie = LeadTheSortie QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

leadTheSortie :: QuestCard LeadTheSortie
leadTheSortie = quest LeadTheSortie Cards.leadTheSortie

instance HasModifiersFor LeadTheSortie where
  getModifiersFor (LeadTheSortie a) = when a.flipped do
    modified a GameTarget [WinAtRoundEndWithVictoryCard $ toCardCode Enemies.lieutenantOfAngmar]
    whenM (selectAny $ LocationWithCardCode $ toCardCode Locations.batteringRam) $
      modifySelf a [ProgressLimit 0]

instance HasAbilities LeadTheSortie where
  getAbilities (LeadTheSortie a) =
    [ restricted a 1 (EpicTargetsExist EpicOtherNonUniqueNoVictoryEnemy)
      $ ResponseAbility RoundEndsWindow $ Costs [EachPlayerThreatCost 1, EncounterRevealCost 1]
    | a.flipped]

instance RunMessage LeadTheSortie where
  runMessage msg q@(LeadTheSortie a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      LeadTheSortie <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      addSetAsideToStaging $ cardIs Locations.batteringRam
      addSetAsideToStaging $ cardIs Enemies.lieutenantOfAngmar
      lead <- getLead
      eachPlayer $ \pid -> when (pid /= lead) $ revealEncounterCardAs pid
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      chooseEpicTarget a lead EpicOtherNonUniqueNoVictoryEnemy EpicDiscardEnemy
      pure q
    _ -> LeadTheSortie <$> runMessage msg a
