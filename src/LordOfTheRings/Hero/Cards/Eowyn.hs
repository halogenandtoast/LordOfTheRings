module LordOfTheRings.Hero.Cards.Eowyn (eowyn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Eowyn = Eowyn HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eowyn :: HeroCard Eowyn
eowyn = hero Eowyn Cards.eowyn

-- | Action: Discard 1 card from your hand to give Éowyn +1 willpower until the end of the
-- phase. This effect may be triggered by each player once each round.
instance HasAbilities Eowyn where
  getAbilities (Eowyn a) =
    [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost a.owner 1)]

instance RunMessage Eowyn where
  runMessage msg h@(Eowyn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ CreateEffect $ Effect (toSource attrs) (toTarget attrs) [WillpowerModifier 1] UntilEndOfPhase
      pure h
    _ -> Eowyn <$> runMessage msg attrs
