module LordOfTheRings.Ally.Cards.LongbeardOrcSlayer (longbeardOrcSlayer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype LongbeardOrcSlayer = LongbeardOrcSlayer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longbeardOrcSlayer :: AllyCard LongbeardOrcSlayer
longbeardOrcSlayer = ally LongbeardOrcSlayer Cards.longbeardOrcSlayer

-- | Response: After Longbeard Orc Slayer enters play, deal 1 damage to each Orc enemy in play.
instance HasAbilities LongbeardOrcSlayer where
  getAbilities (LongbeardOrcSlayer a) =
    [restricted a 1 (exists $ EnemyWithTrait Orc) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage LongbeardOrcSlayer where
  runMessage msg l@(LongbeardOrcSlayer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      orcs <- select (EnemyWithTrait Orc)
      pushAll [DealDamage (toSource attrs) (toTarget e) 1 | e <- orcs]
      pure l
    _ -> LongbeardOrcSlayer <$> runMessage msg attrs
