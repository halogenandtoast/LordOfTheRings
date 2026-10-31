module LordOfTheRings.Treachery.Cards.ShotFromTheSky (shotFromTheSky) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Eagle))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShotFromTheSky = ShotFromTheSky TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

shotFromTheSky :: TreacheryCard ShotFromTheSky
shotFromTheSky = treachery ShotFromTheSky Cards.shotFromTheSky

-- | Counts as a Condition attachment with the text: "Limit 1 per character. Attached
-- character loses the Eagle trait, and takes 1 damage each time it readies."
instance HasModifiersFor ShotFromTheSky where
  getModifiersFor (ShotFromTheSky a) = case a.placement of
    AttachedTo host -> modified a host [LoseTrait Eagle]
    _ -> pure ()

instance HasAbilities ShotFromTheSky where
  getAbilities (ShotFromTheSky a) = case a.placement of
    AttachedTo host -> [mkAbility a 1 $ forced $ BecameReadyWindow After host]
    _ -> []

-- | Surge.
-- When Revealed: Attach to an Eagle character.
-- Shadow: Resolve the "when revealed" effect of Shot from the Sky.
instance RunMessage ShotFromTheSky where
  runMessage msg t@(ShotFromTheSky attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attachToEagle
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      attachToEagle
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> dealDamage attrs host 1
      pure t
    _ -> ShotFromTheSky <$> runMessage msg attrs
   where
    attachToEagle = do
      eagles <- selectCharacters (HeroWithTrait Eagle) (AllyWithTrait Eagle)
      free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode) eagles
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "shotFromTheSky.attach"
        causedBy attrs
        targets free $ attachTreachery attrs.treacheryId
