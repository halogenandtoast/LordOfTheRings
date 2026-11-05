module LordOfTheRings.Treachery.Cards.HaveYouSeenBaggins (haveYouSeenBaggins) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HaveYouSeenBaggins = HaveYouSeenBaggins TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Surge.
haveYouSeenBaggins :: TreacheryCard HaveYouSeenBaggins
haveYouSeenBaggins = treachery HaveYouSeenBaggins Cards.haveYouSeenBaggins

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Forced: After a
-- player fails a Hide test, discard attached hero."
instance HasAbilities HaveYouSeenBaggins where
  getAbilities (HaveYouSeenBaggins a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced $ FailedHideTestWindow After AnyPlayer]
    _ -> []

-- | When Revealed: Attach to a hero, one without a copy: the player who revealed it
-- chooses, on his own (Peril).
instance RunMessage HaveYouSeenBaggins where
  runMessage msg t@(HaveYouSeenBaggins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      taken <- hostsOfTreachery (toCardCode Cards.haveYouSeenBaggins)
      heroes <- filter (\h -> not $ any (isTarget h) taken) <$> select AnyHero
      chooseOrRunOneM you do
        questionLabeled "haveYouSeenBaggins.attach"
        causedBy attrs
        targets heroes $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs (discard attrs)
      pure t
    _ -> HaveYouSeenBaggins <$> runMessage msg attrs
