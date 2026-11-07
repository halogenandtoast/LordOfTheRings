module LordOfTheRings.Quest.Cards.AnnuminasFinale (leadTheSortie, stopTheAttack) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (EpicEffect (EpicDiscardEnemy), EpicTargetQuery (EpicOtherNonUniqueNoVictoryEnemy))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnnuminasFinale = AnnuminasFinale QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

leadTheSortie, stopTheAttack :: QuestCard AnnuminasFinale
leadTheSortie = quest AnnuminasFinale Cards.leadTheSortie
stopTheAttack = quest AnnuminasFinale Cards.stopTheAttack

blockingLocation :: QuestAttrs -> CardDef
blockingLocation a
  | toCardCode a == toCardCode Cards.stopTheAttack = Locations.siegeTunnel
  | otherwise = Locations.batteringRam

instance HasModifiersFor AnnuminasFinale where
  getModifiersFor (AnnuminasFinale a) = when a.flipped do
    modified a GameTarget [WinAtRoundEndWithVictoryCard $ toCardCode Enemies.lieutenantOfAngmar]
    whenM (selectAny $ LocationWithCardCode $ toCardCode $ blockingLocation a) $
      modifySelf a [ProgressLimit 0]

instance HasAbilities AnnuminasFinale where
  getAbilities (AnnuminasFinale a) =
    [ restricted a 1 (EpicTargetsExist EpicOtherNonUniqueNoVictoryEnemy)
      $ ResponseAbility RoundEndsWindow $ Costs [EachPlayerThreatCost 1, EncounterRevealCost 1]
    | a.flipped]

instance RunMessage AnnuminasFinale where
  runMessage msg q@(AnnuminasFinale a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      AnnuminasFinale <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      addSetAsideToStaging $ cardIs $ blockingLocation a
      when (toCardCode a == toCardCode Cards.leadTheSortie) $
        addSetAsideToStaging $ cardIs Enemies.lieutenantOfAngmar
      lead <- getLead
      eachPlayer $ \pid -> when (pid /= lead) $ revealEncounterCardAs pid
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      chooseEpicTarget a lead EpicOtherNonUniqueNoVictoryEnemy EpicDiscardEnemy
      pure q
    _ -> AnnuminasFinale <$> runMessage msg a
