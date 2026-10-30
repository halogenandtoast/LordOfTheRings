module LordOfTheRings.Enemy.Cards.RuthlessHillTroll (ruthlessHillTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Safe))

newtype RuthlessHillTroll = RuthlessHillTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruthlessHillTroll :: EnemyCard RuthlessHillTroll
ruthlessHillTroll = enemy RuthlessHillTroll Cards.ruthlessHillTroll

-- | Ruthless Hill-troll gets +2 attack and +2 defense while it has 3 or more damage on it.
instance HasModifiersFor RuthlessHillTroll where
  getModifiersFor (RuthlessHillTroll a) =
    when (a.damage >= 3) $ modified a a [AttackModifier 2, DefenseModifier 2]

-- | Shadow: Shuffle 1 location with the safe keyword (but not its attached encounter) back
-- into the encounter deck. The location's guard stays where it is.
instance RunMessage RuthlessHillTroll where
  runMessage msg e@(RuthlessHillTroll attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      safe <- select (LocationWithKeyword Safe)
      chooseOrRunOneM pid do
        questionLabeled "ruthlessHillTroll.shuffle"
        causedBy attrs
        targets safe shuffleIntoEncounterDeck
      pure e
    _ -> RuthlessHillTroll <$> runMessage msg attrs
