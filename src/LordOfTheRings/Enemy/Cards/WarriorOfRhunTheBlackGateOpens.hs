module LordOfTheRings.Enemy.Cards.WarriorOfRhunTheBlackGateOpens (warriorOfRhunTheBlackGateOpens) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WarriorOfRhunTheBlackGateOpens = WarriorOfRhunTheBlackGateOpens EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

warriorOfRhunTheBlackGateOpens :: EnemyCard WarriorOfRhunTheBlackGateOpens
warriorOfRhunTheBlackGateOpens = enemy WarriorOfRhunTheBlackGateOpens Cards.warriorOfRhunTheBlackGateOpens

-- | When Revealed: Choose a questing character you control and remove it from the quest.
-- Shadow: Defending character cannot ready until the end of the round.
instance RunMessage WarriorOfRhunTheBlackGateOpens where
  runMessage msg e@(WarriorOfRhunTheBlackGateOpens attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      questing <- selectCharacters (HeroControlledBy (PlayerWithId you) <> CommittedHero) (AllyControlledBy (PlayerWithId you) <> CommittedAlly)
      chooseOrRunOneM you do
        questionLabeled "warriorOfRhunTheBlackGateOpens.remove"
        causedBy attrs
        targets questing removeFromQuest
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d CannotReady
      pure e
    _ -> WarriorOfRhunTheBlackGateOpens <$> runMessage msg attrs
