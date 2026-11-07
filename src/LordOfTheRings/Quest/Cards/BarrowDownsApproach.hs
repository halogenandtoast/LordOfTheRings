module LordOfTheRings.Quest.Cards.BarrowDownsApproach (theHouseOfTomBombadil, acrossTheDowns) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheHouseOfTomBombadil = TheHouseOfTomBombadil QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theHouseOfTomBombadil :: QuestCard TheHouseOfTomBombadil
theHouseOfTomBombadil = quest TheHouseOfTomBombadil Cards.theHouseOfTomBombadil

instance HasModifiersFor TheHouseOfTomBombadil where
  getModifiersFor (TheHouseOfTomBombadil a) = when a.flipped $ modified a GameTarget [SkipQuestPhase]

instance HasAbilities TheHouseOfTomBombadil where
  getAbilities (TheHouseOfTomBombadil a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

instance RunMessage TheHouseOfTomBombadil where
  runMessage msg q@(TheHouseOfTomBombadil a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- Each physical copy is set aside: five Great Barrows and the two unique locations.
      let waiting = map toCardCode [Locations.greatBarrow, Locations.standingStones, Locations.hollowCircle]
      traverse_ setAside $ filter ((`elem` waiting) . toCardCode) deck
      shuffleEncounterDeck
      turnOver a
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> q <$ placeProgress a a 1
    _ -> TheHouseOfTomBombadil <$> runMessage msg a

newtype AcrossTheDowns = AcrossTheDowns QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheDowns :: QuestCard AcrossTheDowns
acrossTheDowns = quest AcrossTheDowns Cards.acrossTheDowns

instance HasModifiersFor AcrossTheDowns where
  getModifiersFor (AcrossTheDowns a) = when a.flipped do
    victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    unless (any ((== toCardCode Locations.hollowCircle) . toCardCode) victory) $
      modifySelf a [CannotBeDefeated]

instance HasAbilities AcrossTheDowns where
  getAbilities (AcrossTheDowns a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.flipped]

instance RunMessage AcrossTheDowns where
  runMessage msg q@(AcrossTheDowns a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      AcrossTheDowns <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      addSetAsideToStaging $ cardIs Locations.hollowCircle
      eachPlayer revealEncounterCardAs
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer $ \pid -> encounterRaisesThreat pid 1
      pure q
    _ -> AcrossTheDowns <$> runMessage msg a
