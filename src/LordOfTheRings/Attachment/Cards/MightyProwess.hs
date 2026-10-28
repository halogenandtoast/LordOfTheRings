module LordOfTheRings.Attachment.Cards.MightyProwess (mightyProwess) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype MightyProwess = MightyProwess AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a tactics hero. Limit 1 per hero.
mightyProwess :: AttachmentCard MightyProwess
mightyProwess = attachment MightyProwess Cards.mightyProwess & attachTo (AttachToHero $ HeroWithSphere Tactics)

-- | Response: After attached hero attacks and destroys an enemy, deal 1 damage to another
-- enemy that shares a Trait with the enemy just destroyed.
instance HasAbilities MightyProwess where
  getAbilities (MightyProwess a) =
    [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage MightyProwess where
  runMessage msg s@(MightyProwess a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      let traits = concat [toList ts | Window _ (EnemyDestroyedByAttack _ _ ts) <- windows]
      enemies <- ordNub . concat <$> traverse (select . EnemyWithTrait) traits
      chooseOneM pid do
        questionLabeled "mightyProwess.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> MightyProwess <$> runMessage msg a
