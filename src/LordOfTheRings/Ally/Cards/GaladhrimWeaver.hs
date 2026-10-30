module LordOfTheRings.Ally.Cards.GaladhrimWeaver (galadhrimWeaver) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GaladhrimWeaver = GaladhrimWeaver AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadhrimWeaver :: AllyCard GaladhrimWeaver
galadhrimWeaver = ally GaladhrimWeaver Cards.galadhrimWeaver

-- | Cannot attack or defend.
instance HasModifiersFor GaladhrimWeaver where
  getModifiersFor (GaladhrimWeaver a) = modifySelf a [CannotAttackOrDefend]

-- | Response: After Galadhrim Weaver enters play, shuffle the top card of your discard
-- pile into your deck.
instance HasAbilities GaladhrimWeaver where
  getAbilities (GaladhrimWeaver a) = [restricted a 1 (CardInDiscard a.owner AnyCard) $ response $ entersPlay a]

instance RunMessage GaladhrimWeaver where
  runMessage msg s@(GaladhrimWeaver a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- headMay . (.discard) <$> getPlayer pid
      for_ top \c -> shuffleFromDiscardIntoDeck pid c.id
      pure s
    _ -> GaladhrimWeaver <$> runMessage msg a
