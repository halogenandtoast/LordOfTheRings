module LordOfTheRings.Quest.Cards.RebuildTheDefenses (rebuildTheDefenses) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (repairAnnuminas)

newtype RebuildTheDefenses = RebuildTheDefenses QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rebuildTheDefenses :: QuestCard RebuildTheDefenses
rebuildTheDefenses = quest RebuildTheDefenses Cards.rebuildTheDefenses

instance HasAbilities RebuildTheDefenses where
  getAbilities (RebuildTheDefenses a)
    | a.flipped =
        [ mkAbility a 1 $ forced RoundBeginsWindow
        , mkAbility a 2 $ ResponseAbility RoundEndsWindow (QuestProgressCost a.id 10)
        ]
    | otherwise = []

instance RunMessage RebuildTheDefenses where
  runMessage msg q@(RebuildTheDefenses a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let waiting = map toCardCode
            [ Enemies.lieutenantOfAngmar, Locations.batteringRam, Locations.enemyCamp
            , Locations.siegeTunnel, Enemies.hostOfAngmar, Objectives.dunedainOfAnnuminas
            ]
      traverse_ setAside $ filter ((`elem` waiting) . toCardCode) deck
      players <- getPlayerOrder
      push $ SearchDifferentEncounterCardsIncluding (toCardCode Locations.gateOfAnnuminas)
        players (toTarget a) (CardWithType LocationType) False []
      turnOver a
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Resource a.questTokens >= 4) advanceQuest
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> q <$ repairAnnuminas a 10
    _ -> RebuildTheDefenses <$> runMessage msg a
