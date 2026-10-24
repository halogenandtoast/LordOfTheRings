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
    [ anyPlayer
        $ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithCardsInHand 1) (ActionAbility Free)
    ]

instance RunMessage Eowyn where
  runMessage msg h@(Eowyn attrs) = case msg of
    -- the player who triggers it discards from their own hand
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseAndDiscardFromHand pid attrs 1
      phaseModifier attrs attrs (WillpowerModifier 1)
      pure h
    _ -> Eowyn <$> runMessage msg attrs
