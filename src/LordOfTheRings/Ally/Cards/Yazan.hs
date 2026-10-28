module LordOfTheRings.Ally.Cards.Yazan (yazan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Yazan = Yazan AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, as its card def says.
yazan :: AllyCard Yazan
yazan = ally Yazan Cards.yazan

-- | Response: After Yazan is declared as an attacker, deal 1 damage to a non-unique enemy
-- in play. (Limit once per phase.)
instance HasAbilities Yazan where
  getAbilities (Yazan a) =
    [ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (exists $ not_ UniqueEnemy)
        $ response
        $ AttackerDeclaredWindow After (Just $ toTarget a)
    ]

instance RunMessage Yazan where
  runMessage msg s@(Yazan a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (not_ UniqueEnemy)
      chooseOneM pid do
        questionLabeled "yazan.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> Yazan <$> runMessage msg a
