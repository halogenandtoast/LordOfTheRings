module LordOfTheRings.Hero.Cards.Erestor (erestor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Erestor = Erestor HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

erestor :: HeroCard Erestor
erestor = hero Erestor Cards.erestor

-- | Draw 3 additional cards at the beginning of the resource phase.
instance HasModifiersFor Erestor where
  getModifiersFor (Erestor a) = modified a (PlayerTarget a.owner) [AdditionalResourcePhaseDraws 3]

-- | At the end of the round, discard all cards in your hand.
instance HasAbilities Erestor where
  getAbilities (Erestor a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage Erestor where
  runMessage msg h@(Erestor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardHand attrs.owner
      pure h
    _ -> Erestor <$> runMessage msg attrs
