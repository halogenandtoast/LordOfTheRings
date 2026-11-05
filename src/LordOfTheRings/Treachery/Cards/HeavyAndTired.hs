module LordOfTheRings.Treachery.Cards.HeavyAndTired (heavyAndTired) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HeavyAndTired = HeavyAndTired TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

heavyAndTired :: TreacheryCard HeavyAndTired
heavyAndTired = treachery HeavyAndTired Cards.heavyAndTired

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Attached hero cannot
-- commit to Fortitude tests."
instance HasModifiersFor HeavyAndTired where
  getModifiersFor (HeavyAndTired a) = case a.placement of
    AttachedTo host -> modified a host [CannotCommitToFortitudeTests]
    _ -> pure ()

-- | "Forced: After you fail a Fortitude test, exhaust attached hero and discard each
-- non-objective attachment on it." You are the attached hero's controller.
instance HasAbilities HeavyAndTired where
  getAbilities (HeavyAndTired a) = case a.placement of
    AttachedTo (HeroTarget hid) -> [mkAbility a 1 $ forced $ FailedFortitudeTestWindow After (ControllerOfHero hid)]
    _ -> []

-- | When Revealed: Attach to the hero with the most attachments and exhaust that hero. A
-- hero with Heavy and Tired attached already cannot take another; the first player
-- chooses among heroes tied for the most.
instance RunMessage HeavyAndTired where
  runMessage msg t@(HeavyAndTired attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . HeroTarget) =<< select AnyHero
      counted <- for heroes \h -> (h,) . length <$> getAttachmentsOn (HeroTarget h)
      let most = foldr (max . snd) 0 counted
          hosts = [h | (h, n) <- counted, n == most]
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "heavyAndTired.attach"
        causedBy attrs
        targets hosts \h -> do
          attachTreachery attrs.treacheryId (HeroTarget h)
          exhaust (HeroTarget h)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> case attrs.placement of
      AttachedTo host -> do
        exhaust host
        attachments <- getAttachmentsOn host
        for_ [a | a <- attachments, not (isObjective a)] (discard attrs)
        pure t
      _ -> pure t
    _ -> HeavyAndTired <$> runMessage msg attrs
   where
    isObjective = \case
      ObjectiveTarget _ -> True
      _ -> False
