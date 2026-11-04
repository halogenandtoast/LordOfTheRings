module LordOfTheRings.Attachment.Cards.ArmoredDestrier (armoredDestrier) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype ArmoredDestrier = ArmoredDestrier AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a leadership or sentinel hero. Restricted, on its card def.
armoredDestrier :: AttachmentCard ArmoredDestrier
armoredDestrier =
  attachment ArmoredDestrier Cards.armoredDestrier
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithSphere Leadership, HeroWithKeyword Sentinel])

-- | Response: After attached hero defends against an attack, exhaust Armored Destrier to
-- ready attached hero. Then, discard a shadow card from another enemy engaged with the
-- defending player.
instance HasAbilities ArmoredDestrier where
  getAbilities (ArmoredDestrier a) =
    [mkAbility a 1 $ ResponseAbility (defendedAttack host) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage ArmoredDestrier where
  runMessage msg s@(ArmoredDestrier a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      withHost a ready
      -- the defending player is the one the attacker is engaged with
      for_ (take 1 [eid | Window _ (DefendedAttack _ eid) <- windows]) \eid -> do
        defending <- placementController . (.placement) <$> getEnemy eid
        for_ defending \p -> do
          others <- select (EnemyEngagedWith (PlayerWithId p) <> not_ (EnemyWithId eid))
          shadowed <- filterM (fmap notNull . (shadowsPlayerCardsCanAffect <=< getEnemy)) others
          chooseOrRunOneM pid do
            questionLabeled "armoredDestrier.enemy"
            causedBy a
            targets shadowed \e -> do
              shadows <- shadowsPlayerCardsCanAffect =<< getEnemy e
              chooseOrRunOneM pid do
                questionLabeled "armoredDestrier.shadow"
                causedBy a
                targets shadows (discardShadowCard e)
      pure s
    _ -> ArmoredDestrier <$> runMessage msg a
