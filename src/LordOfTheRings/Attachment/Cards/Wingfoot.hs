module LordOfTheRings.Attachment.Cards.Wingfoot (wingfoot) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (..))
import LordOfTheRings.Trait (Trait (Ranger))

newtype Wingfoot = Wingfoot AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wingfoot :: AttachmentCard Wingfoot
wingfoot = attachment Wingfoot Cards.wingfoot & attachTo (AttachToHero $ HeroWithTrait Ranger)

-- | Attach to a Ranger hero. Response: After attached hero commits to a quest, name
-- enemy, location or treachery. If a card of the named type is revealed during this
-- quest phase, exhaust Wingfoot to ready attached hero.
--
-- The named type stays on Wingfoot until the end of the phase; each type's reveal is an
-- ability of its own, open only while that type is named.
instance HasAbilities Wingfoot where
  getAbilities (Wingfoot a) = case a.placement of
    AttachedTo host@(HeroTarget hid) ->
      mkAbility a 1 (response $ CommittedToQuestWindow After host)
        : [ restricted a n (TargetHasModifier (toTarget a) (NamedCardType cType) <> exists (HeroWithId hid <> ExhaustedHero))
            $ ResponseAbility window (ExhaustCost $ toTarget a)
          | (n, cType, window) <-
              [ (2, EnemyType, EnemyRevealedWindow After)
              , (3, LocationType, LocationRevealedWindow After)
              , (4, TreacheryType, TreacheryRevealedWindow After)
              ]
          ]
    _ -> []

instance RunMessage Wingfoot where
  runMessage msg s@(Wingfoot a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseOneM pid do
        questionLabeled "wingfoot.name"
        causedBy a
        labeled "wingfoot.enemy" $ phaseModifier a a (NamedCardType EnemyType)
        labeled "wingfoot.location" $ phaseModifier a a (NamedCardType LocationType)
        labeled "wingfoot.treachery" $ phaseModifier a a (NamedCardType TreacheryType)
      pure s
    UseCardAbility _ (isSource a -> True) n _ | n `elem` [2, 3, 4] -> do
      withHost a ready
      pure s
    _ -> Wingfoot <$> runMessage msg a
