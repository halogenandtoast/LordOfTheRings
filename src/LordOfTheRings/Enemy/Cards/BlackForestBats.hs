module LordOfTheRings.Enemy.Cards.BlackForestBats (blackForestBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BlackForestBats = BlackForestBats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackForestBats :: EnemyCard BlackForestBats
blackForestBats = enemy BlackForestBats Cards.blackForestBats

-- | When Revealed: Each player must choose 1 character currently committed to a quest, and
-- remove that character from the quest. (The chosen character does not ready.)
instance RunMessage BlackForestBats where
  runMessage msg e@(BlackForestBats attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        committed <- selectCharacters (HeroControlledBy (PlayerWithId p) <> CommittedHero) (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        chooseOneM p do
          questionLabeled "blackForestBats.remove"
          causedBy attrs
          targets committed \c -> push $ RemoveFromQuest c
      pure e
    _ -> BlackForestBats <$> runMessage msg attrs
