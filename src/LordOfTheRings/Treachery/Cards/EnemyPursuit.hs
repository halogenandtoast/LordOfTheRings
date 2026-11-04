module LordOfTheRings.Treachery.Cards.EnemyPursuit (enemyPursuit) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EnemyPursuit = EnemyPursuit TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

enemyPursuit :: TreacheryCard EnemyPursuit
enemyPursuit = treachery EnemyPursuit Cards.enemyPursuit

{- | When Revealed: Either remove all progress from the main quest, or assign X damage among
characters in play. X is the number of progress tokens on the main quest. The first player
chooses, and assigns the damage; with no progress on the main quest neither does anything.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage EnemyPursuit where
  runMessage msg t@(EnemyPursuit attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- getMainQuestProgress
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      when (x > 0) $ chooseOneM lead do
        questionLabeled "enemyPursuit.choose"
        causedBy attrs
        labeled "enemyPursuit.removeProgress" $ removeProgressFromMainQuest attrs x
        labeled "enemyPursuit.damage" $ dealDamageDividedAmong attrs lead characters x
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> EnemyPursuit <$> runMessage msg attrs
