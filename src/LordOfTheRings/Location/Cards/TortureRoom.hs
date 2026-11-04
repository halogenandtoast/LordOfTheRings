module LordOfTheRings.Location.Cards.TortureRoom (tortureRoom) where

import LordOfTheRings.Card (cdName, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (discardUntilAlly, guardingAnObjective)

newtype TortureRoom = TortureRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tortureRoom :: LocationCard TortureRoom
tortureRoom = location TortureRoom Cards.tortureRoom

-- | While Torture Room is guarding an objective, it gains: "Forced: At the end of the
-- round, raise each player's threat by 1." Whether it is guarding one is read as the round
-- ends.
instance HasAbilities TortureRoom where
  getAbilities (TortureRoom a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | When Revealed: The first player discards cards from the top of his deck until he
-- discards an ally. Attach that ally to Torture Room and discard each other copy of that
-- ally from play.
instance RunMessage TortureRoom where
  runMessage msg l@(TortureRoom a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      found <- discardUntilAlly lead (const $ pure True)
      for_ found \card -> do
        guardCardWith card a
        traverse_ discard =<< select (AllyWithTitle $ cdName $ toCardDef card)
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      whenM (guardingAnObjective a) $ eachPlayer \p -> encounterRaisesThreat p 1
      pure l
    _ -> TortureRoom <$> runMessage msg a
