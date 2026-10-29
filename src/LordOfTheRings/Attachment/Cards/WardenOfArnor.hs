module LordOfTheRings.Attachment.Cards.WardenOfArnor (wardenOfArnor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Scout))

newtype WardenOfArnor = WardenOfArnor AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Scout hero.
wardenOfArnor :: AttachmentCard WardenOfArnor
wardenOfArnor = attachment WardenOfArnor Cards.wardenOfArnor & attachTo (AttachToHero $ HeroWithTrait Scout)

-- | While attached character is committed to the quest, place 1 progress on the first
-- location revealed by the encounter deck each round. That is the first revealed each
-- round, its hero committed or not.
instance HasAbilities WardenOfArnor where
  getAbilities (WardenOfArnor a) =
    [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ silent $ LocationRevealedWindow After
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage WardenOfArnor where
  runMessage msg s@(WardenOfArnor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      committed <- case a.placement of
        AttachedTo (HeroTarget hid) -> selectAny (HeroWithId hid <> CommittedHero)
        _ -> pure False
      when committed $ for_ (take 1 [lid | Window _ (Revealed (LocationTarget lid)) <- windows]) \lid ->
        placeProgress a lid 1
      pure s
    _ -> WardenOfArnor <$> runMessage msg a
