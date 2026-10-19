module LordOfTheRings.Enemy.Cards.DolGuldurBeastmaster (dolGuldurBeastmaster) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DolGuldurBeastmaster = DolGuldurBeastmaster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolGuldurBeastmaster :: EnemyCard DolGuldurBeastmaster
dolGuldurBeastmaster = enemy DolGuldurBeastmaster Cards.dolGuldurBeastmaster

-- | Forced: When Dol Guldur Beastmaster attacks, deal it 1 additional shadow card.
instance HasAbilities DolGuldurBeastmaster where
  getAbilities (DolGuldurBeastmaster a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage DolGuldurBeastmaster where
  runMessage msg e@(DolGuldurBeastmaster attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealShadowCard attrs.id
      pure e
    _ -> DolGuldurBeastmaster <$> runMessage msg attrs
