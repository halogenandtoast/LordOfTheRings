module LordOfTheRings.Enemy.Cards.CravenEagle (cravenEagle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getHitPoints)

newtype CravenEagle = CravenEagle EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cravenEagle :: EnemyCard CravenEagle
cravenEagle = enemy CravenEagle Cards.cravenEagle

-- | When Revealed: Discard the character with the fewest remaining hit points. That
-- character's controller may discard 3 cards at random from his hand to prevent this
-- effect.
-- The first player chooses among characters tied for the fewest.
instance RunMessage CravenEagle where
  runMessage msg e@(CravenEagle attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      remaining <- for characters \c -> (,c) <$> ((-) <$> getHitPoints c <*> getDamage c)
      let fewest = [c | (r, c) <- remaining, Just r == minimumMay (map fst remaining)]
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "cravenEagle.choose"
        causedBy attrs
        targets fewest \c -> do
          controller <- characterController c
          handSize <- maybe (pure 0) (fmap (length . (.hand)) . getPlayer) controller
          case controller of
            Just pid | handSize >= 3 -> chooseOneM pid do
              questionLabeled "cravenEagle.prevent"
              causedBy attrs
              aimedAt [c]
              labeled "cravenEagle.discardCards" $ discardRandomFromHand pid 3
              labeled "cravenEagle.discardCharacter" $ discard attrs c
            _ -> discard attrs c
      pure e
    _ -> CravenEagle <$> runMessage msg attrs
