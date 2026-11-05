module LordOfTheRings.Treachery.Cards.SpiderBite (spiderBite) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SpiderBite = SpiderBite TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

spiderBite :: TreacheryCard SpiderBite
spiderBite = treachery SpiderBite Cards.spiderBite

instance HasModifiersFor SpiderBite where
  getModifiersFor (SpiderBite a) = case a.placement of
    AttachedTo _ -> modifySelf a [GainTrait Condition]
    _ -> pure ()

instance HasAbilities SpiderBite where
  getAbilities (SpiderBite a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]
    _ -> []

instance RunMessage SpiderBite where
  runMessage msg t@(SpiderBite a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      heroes <-
        filterM (fmap null . treacheriesOn a.treacheryCardCode . toTarget)
          =<< select (HeroControlledBy AnyPlayer <> NotHero UnconsciousHero)
      eligible <- filterM (fmap (notElem CannotHaveTreacheryAttachments) . getModifiers) heroes
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "spiderBite.attach"
        causedBy a
        targets eligible $ attachTreachery a.id
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \h -> givePoison h 1
      pure t
    MakeUnconscious target | a.placement == AttachedTo target -> do
      discard a a
      pure t
    _ -> SpiderBite <$> runMessage msg a
