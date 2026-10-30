module LordOfTheRings.Hero.Cards.Lanwyn (lanwyn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Lanwyn = Lanwyn HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on her card def.
lanwyn :: HeroCard Lanwyn
lanwyn = hero Lanwyn Cards.lanwyn

-- | Response: After an encounter card with surge is revealed, either ready Lanwyn or give
-- her +2 willpower until the end of the phase. (Limit twice per phase.)
instance HasAbilities Lanwyn where
  getAbilities (Lanwyn a) = [limited (GroupLimit PerPhase 2) $ mkAbility a 1 $ response (SurgeCardRevealedWindow After)]

instance RunMessage Lanwyn where
  runMessage msg h@(Lanwyn attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseOrRunOneM pid do
        questionLabeled "lanwyn.choose"
        causedBy attrs
        when attrs.heroExhausted $ labeled "lanwyn.ready" $ ready attrs
        labeled "lanwyn.willpower" $ phaseModifier attrs attrs (WillpowerModifier 2)
      pure h
    _ -> Lanwyn <$> runMessage msg attrs
