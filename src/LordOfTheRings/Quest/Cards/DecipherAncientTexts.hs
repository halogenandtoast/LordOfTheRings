module LordOfTheRings.Quest.Cards.DecipherAncientTexts (decipherAncientTexts) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (flipToClue)
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype DecipherAncientTexts = DecipherAncientTexts QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
decipherAncientTexts :: QuestCard DecipherAncientTexts
decipherAncientTexts = quest DecipherAncientTexts Cards.decipherAncientTexts

-- | While Decipher Ancient Texts is the current quest, allies get -2 willpower.
instance HasModifiersFor DecipherAncientTexts where
  getModifiersFor (DecipherAncientTexts a) = do
    current <- (== Just (toTarget a)) <$> getCurrentQuest
    when current do
      allies <- select AnyAlly
      for_ allies \ally -> modified a ally [WillpowerModifier (-2)]

-- | Action: Pay 1 resource to place 1 progress on Decipher Ancient Texts. (Any player may
-- trigger this action. Limit 3 times per round.)
instance HasAbilities DecipherAncientTexts where
  getAbilities (DecipherAncientTexts a) =
    [limited (GroupLimit PerRound 3) $ anyPlayer $ mkAbility a 1 $ ActionAbility (SpendResourcesCost Neutral 1)]

-- | When this quest is defeated, flip it over.
instance RunMessage DecipherAncientTexts where
  runMessage msg q@(DecipherAncientTexts attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      flipToClue attrs Objectives.ordersFromAngmar
      DecipherAncientTexts <$> runMessage msg attrs
    _ -> DecipherAncientTexts <$> runMessage msg attrs
