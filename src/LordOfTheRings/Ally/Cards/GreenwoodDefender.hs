module LordOfTheRings.Ally.Cards.GreenwoodDefender (greenwoodDefender) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GreenwoodDefender = GreenwoodDefender AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenwoodDefender :: AllyCard GreenwoodDefender
greenwoodDefender = ally GreenwoodDefender Cards.greenwoodDefender

-- | Response: After Greenwood Defender enters play, it does not exhaust to defend until the
-- end of the round.
instance HasAbilities GreenwoodDefender where
  getAbilities (GreenwoodDefender a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage GreenwoodDefender where
  runMessage msg s@(GreenwoodDefender a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      roundModifier a a DoesNotExhaustToDefend
      pure s
    _ -> GreenwoodDefender <$> runMessage msg a
