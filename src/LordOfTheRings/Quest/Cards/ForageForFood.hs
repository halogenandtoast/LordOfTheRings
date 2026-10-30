module LordOfTheRings.Quest.Cards.ForageForFood (forageForFood) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isBlank)

newtype ForageForFood = ForageForFood QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
forageForFood :: QuestCard ForageForFood
forageForFood = quest ForageForFood Cards.forageForFood

-- | Forced: After 1 or more resources are spent from a hero's resource pool, deal 1 damage
-- to that hero.
-- Response: After this stage is defeated, heal 1 damage from each hero. The players gain
-- by it, so it is taken.
instance RunMessage ForageForFood where
  runMessage msg q@(ForageForFood attrs) = case msg of
    -- ponytail: each spend message is one spend; a group payment that takes its resources
    -- one at a time from the same hero deals more than 1 damage
    RemoveTokens (PlayerSource _) (HeroTarget hid) Resource n | n > 0 -> do
      unlessM (isBlank attrs) $ dealDamage attrs hid 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      unlessM (isBlank attrs) do
        heroes <- select AnyHero
        for_ heroes \h -> healDamage attrs h 1
      ForageForFood <$> runMessage msg attrs
    _ -> ForageForFood <$> runMessage msg attrs
