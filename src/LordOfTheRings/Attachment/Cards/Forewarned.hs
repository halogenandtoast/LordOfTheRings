module LordOfTheRings.Attachment.Cards.Forewarned (forewarned) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype Forewarned = Forewarned AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Permanent, on its card def. Setup: Attach to a Ranger
-- hero: as the Campaign Pool records it on its hero.
forewarned :: AttachmentCard Forewarned
forewarned = attachment Forewarned Cards.forewarned & attachTo (AttachToHero $ HeroWithTrait Ranger)

-- | Response: After attached hero is declared as a defender against a non-unique enemy,
-- exhaust Forewarned to discard 1 shadow card from that enemy.
instance HasAbilities Forewarned where
  getAbilities (Forewarned a) =
    [ mkAbility a 1 $ ResponseAbility (CharacterDefenderDeclaredWindow After (HeroWithId hid) (NotAlly AnyAlly) (NotEnemy UniqueEnemy)) (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage Forewarned where
  runMessage msg s@(Forewarned a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> do
        e <- getEnemy eid
        shadows <- shadowsPlayerCardsCanAffect e
        chooseOrRunOneM pid do
          questionLabeled "forewarned.discardShadow"
          causedBy a
          targets shadows \c -> discardShadowCard eid c
      pure s
    _ -> Forewarned <$> runMessage msg a
