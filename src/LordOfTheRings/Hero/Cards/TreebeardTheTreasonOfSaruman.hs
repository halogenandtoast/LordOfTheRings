module LordOfTheRings.Hero.Cards.TreebeardTheTreasonOfSaruman (treebeardTheTreasonOfSaruman) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype TreebeardTheTreasonOfSaruman = TreebeardTheTreasonOfSaruman HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

treebeardTheTreasonOfSaruman :: HeroCard TreebeardTheTreasonOfSaruman
treebeardTheTreasonOfSaruman = hero TreebeardTheTreasonOfSaruman Cards.treebeardTheTreasonOfSaruman

-- | Cannot have restricted attachments.
instance HasModifiersFor TreebeardTheTreasonOfSaruman where
  getModifiersFor (TreebeardTheTreasonOfSaruman a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Action: Deal 1 damage to Treebeard to give him +1 [willpower] and +1 [attack] until the
-- end of the phase. (Limit 5 times per phase.)
instance HasAbilities TreebeardTheTreasonOfSaruman where
  getAbilities (TreebeardTheTreasonOfSaruman a) =
    [limited (PlayerLimit PerPhase 5) $ mkAbility a 1 $ ActionAbility (DamageHeroCost (HeroWithId a.id) 1)]

instance RunMessage TreebeardTheTreasonOfSaruman where
  runMessage msg h@(TreebeardTheTreasonOfSaruman attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      phaseModifiers attrs attrs [WillpowerModifier 1, AttackModifier 1]
      pure h
    _ -> TreebeardTheTreasonOfSaruman <$> runMessage msg attrs
