module LordOfTheRings.Attachment.Cards.StaffOfLebethron (staffOfLebethron) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor, Hobbit))

newtype StaffOfLebethron = StaffOfLebethron AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor or Hobbit hero. Restricted.
staffOfLebethron :: AttachmentCard StaffOfLebethron
staffOfLebethron =
  attachment StaffOfLebethron Cards.staffOfLebethron
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Gondor, HeroWithTrait Hobbit])

-- | Response: After attached hero exhausts to defend an attack from an enemy with
-- engagement cost higher than your threat, exhaust Staff of Lebethron to discard a shadow
-- card from that enemy. Exhausted to defend, the hero is exhausted once declared.
instance HasAbilities StaffOfLebethron where
  getAbilities (StaffOfLebethron a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero)
        $ ResponseAbility
          (CharacterDefenderDeclaredWindow After (HeroWithId hid) (NotAlly AnyAlly) (EnemyWithEngagementCostAboveThreatOf a.controller <> EnemyWithFacedownShadow))
          (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage StaffOfLebethron where
  runMessage msg s@(StaffOfLebethron a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> do
        shadows <- shadowsPlayerCardsCanAffect =<< getEnemy eid
        chooseOrRunOneM pid do
          questionLabeled "staffOfLebethron.shadow"
          causedBy a
          targets shadows (discardShadowCard eid)
      pure s
    _ -> StaffOfLebethron <$> runMessage msg a
