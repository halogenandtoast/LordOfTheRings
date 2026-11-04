module LordOfTheRings.Attachment.Cards.DurinsAxeTactics (durinsAxeTactics) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DurinsAxeTactics = DurinsAxeTactics AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy) and Restricted, on its card def. Attach to a hero.
durinsAxeTactics :: AttachmentCard DurinsAxeTactics
durinsAxeTactics = attachment DurinsAxeTactics Cards.durinsAxeTactics

-- | Attached hero gets +2 attack.
instance HasModifiersFor DurinsAxeTactics where
  getModifiersFor (DurinsAxeTactics a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached hero attacks and destroys an enemy, deal 1 damage to an enemy
-- engaged with you.
instance HasAbilities DurinsAxeTactics where
  getAbilities (DurinsAxeTactics a) =
    [ restricted a 1 (exists $ EnemyEngagedWith (PlayerWithId a.owner)) $ response $ EnemyDestroyedByAttackWindow After (Just host)
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

instance RunMessage DurinsAxeTactics where
  runMessage msg s@(DurinsAxeTactics a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> DurinsAxeTactics <$> runMessage msg a
