module LordOfTheRings.Quest.Cards.ThroughTheCaverns (throughTheCaverns) where

import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThroughTheCaverns = ThroughTheCaverns QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheCaverns :: QuestCard ThroughTheCaverns
throughTheCaverns = quest ThroughTheCaverns Cards.throughTheCaverns

-- | The players, as a group, cannot play more than 1 ally card each round.
instance HasModifiersFor ThroughTheCaverns where
  getModifiersFor (ThroughTheCaverns a) = modified a GameTarget [OneAllyPerRound]

-- | Response: After placing any number of progress tokens on this card, flip the
-- "prisoner" hero card face-up, and place 1 damage token on it. This hero has been
-- "rescued", and may now be used by its controller.
instance HasAbilities ThroughTheCaverns where
  getAbilities (ThroughTheCaverns a) =
    [restricted a 1 (HeroExists PrisonerHero) $ response $ ProgressPlacedWindow After (toTarget a)]

-- | Players cannot advance to the next stage unless they have rescued the prisoner and have
-- all 3 "Escape from Dol Guldur" objective cards.
instance RunMessage ThroughTheCaverns where
  runMessage msg q@(ThroughTheCaverns attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      prisoners <- select PrisonerHero
      pushAll $ map RescuePrisoner prisoners
      pure q
    _ -> ThroughTheCaverns <$> runConditionalStage condition msg attrs
   where
    condition = andM [selectNone PrisonerHero, (>= 3) <$> claimedObjectives]
