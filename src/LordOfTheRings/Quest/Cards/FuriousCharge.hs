module LordOfTheRings.Quest.Cards.FuriousCharge (furiousCharge) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (flipThaurdir, thaurdirCaptain)

newtype FuriousCharge = FuriousCharge QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
furiousCharge :: QuestCard FuriousCharge
furiousCharge = quest FuriousCharge Cards.furiousCharge

-- | Forced: When at least 1 progress is placed on Furious Charge, each character gets -2
-- willpower until the end of the round. If Thaurdir is Captain side faceup, flip him.
instance HasAbilities FuriousCharge where
  getAbilities (FuriousCharge a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Response: After Furious Charge is defeated, add 10 progress to the main quest. The
-- players gain by it, so it is taken.
instance RunMessage FuriousCharge where
  runMessage msg q@(FuriousCharge attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      characters <- selectCharacters (HeroControlledBy AnyPlayer) (AllyControlledBy AnyPlayer)
      for_ characters \c -> roundModifier attrs c (WillpowerModifier (-2))
      captain <- selectAny thaurdirCaptain
      when captain flipThaurdir
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      mains <- getMainQuests
      for_ mains \main -> placeProgress attrs main 10
      FuriousCharge <$> runMessage msg attrs
    _ -> FuriousCharge <$> runMessage msg attrs
