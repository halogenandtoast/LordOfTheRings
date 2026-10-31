module LordOfTheRings.Location.Cards.TortureChamberMountGram (tortureChamberMountGram) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder)

newtype TortureChamberMountGram = TortureChamberMountGram LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves. Surge.
tortureChamberMountGram :: LocationCard TortureChamberMountGram
tortureChamberMountGram = location TortureChamberMountGram Cards.tortureChamberMountGram

-- | X is twice the number of cards captured underneath Torture Chamber.
instance HasModifiersFor TortureChamberMountGram where
  getModifiersFor (TortureChamberMountGram a) = do
    n <- length <$> capturedUnder a.cardId
    when (n > 0) $ modified a a [QuestPointModifier (2 * n)]

-- | Forced: At the end of the combat phase, if Torture Chamber is in the staging area, deal
-- 1 damage to each exhausted character.
instance HasAbilities TortureChamberMountGram where
  getAbilities (TortureChamberMountGram a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase | a.placement == StagingArea]

instance RunMessage TortureChamberMountGram where
  runMessage msg l@(TortureChamberMountGram a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 1
      pure l
    _ -> TortureChamberMountGram <$> runMessage msg a
