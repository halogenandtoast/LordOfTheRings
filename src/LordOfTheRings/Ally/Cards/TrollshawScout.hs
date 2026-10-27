module LordOfTheRings.Ally.Cards.TrollshawScout (trollshawScout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype TrollshawScout = TrollshawScout AllyAttrs
  deriving newtype (Show, Eq, ToJSON, Entity)
  deriving anyclass IsAlly

-- | Ranged, as its card def says.
trollshawScout :: AllyCard TrollshawScout
trollshawScout = ally TrollshawScout Cards.trollshawScout

-- | Trollshaw Scout does not exhaust to attack.
instance HasModifiersFor TrollshawScout where
  getModifiersFor (TrollshawScout a) = modifySelf a [DoesNotExhaustToAttack]

-- | Forced: After Trollshaw Scout attacks, either discard it from play or discard
-- 1 card from your hand.
instance HasAbilities TrollshawScout where
  getAbilities (TrollshawScout a) = [mkAbility a 1 $ forced $ attacked a]

instance RunMessage TrollshawScout where
  runMessage msg s@(TrollshawScout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      hand <- (.hand) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "discardItOrFromHand"
        causedBy a
        labeled "discardItFromPlay" $ discard a
        unless (null hand) $ labeled "discardOneFromHand" $ chooseAndDiscardFromHand pid a 1
      pure s
    _ -> TrollshawScout <$> runMessage msg a
