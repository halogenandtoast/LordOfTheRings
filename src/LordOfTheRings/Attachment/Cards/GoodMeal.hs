module LordOfTheRings.Attachment.Cards.GoodMeal (goodMeal) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype GoodMeal = GoodMeal AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero.
goodMeal :: AttachmentCard GoodMeal
goodMeal = attachment GoodMeal Cards.goodMeal & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Action: Discard Good Meal to lower the cost of the next event you play this round
-- that matches attached hero's sphere by 2.
instance HasAbilities GoodMeal where
  getAbilities (GoodMeal a) = [mkAbility a 1 $ ActionAbility Free | AttachedTo (HeroTarget _) <- [a.placement]]

instance RunMessage GoodMeal where
  runMessage msg s@(GoodMeal a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        -- a sphere icon the hero gains is a sphere it belongs to (ruling 3.065)
        printed <- (.sphere) <$> getHero hid
        gained <- (\ms -> [s' | GainResourceIcon s' <- ms]) <$> getModifiers (HeroTarget hid)
        discard a a
        roundModifier a (PlayerTarget pid)
          $ LowerNextCostOf (CardWithType EventType <> CardMatchAny (map CardWithSphere $ printed : gained)) 2
      pure s
    _ -> GoodMeal <$> runMessage msg a
