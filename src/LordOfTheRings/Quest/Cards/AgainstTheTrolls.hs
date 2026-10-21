module LordOfTheRings.Quest.Cards.AgainstTheTrolls (againstTheTrolls) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))

newtype AgainstTheTrolls = AgainstTheTrolls QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

againstTheTrolls :: QuestCard AgainstTheTrolls
againstTheTrolls = quest AgainstTheTrolls Cards.againstTheTrolls

-- | When Revealed: Place the unique Troll cards previously set aside into the staging
-- area. Players cannot defeat this stage if there are any Troll enemies in play.
instance RunMessage AgainstTheTrolls where
  runMessage msg q@(AgainstTheTrolls attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      AgainstTheTrolls <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      push $ AddSetAsideToStaging (CardWithTrait Troll)
      pure q
    _ -> AgainstTheTrolls <$> runBlockedStage (EnemyWithTrait Troll) msg attrs
