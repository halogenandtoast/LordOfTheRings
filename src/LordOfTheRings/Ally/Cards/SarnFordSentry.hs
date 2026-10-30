module LordOfTheRings.Ally.Cards.SarnFordSentry (sarnFordSentry) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SarnFordSentry = SarnFordSentry AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sarnFordSentry :: AllyCard SarnFordSentry
sarnFordSentry = ally SarnFordSentry Cards.sarnFordSentry

-- | Response: After Sarn Ford Sentry enters play, draw 1 card for each enemy engaged with
-- you.
instance HasAbilities SarnFordSentry where
  getAbilities (SarnFordSentry a) =
    [restricted a 1 (exists $ EnemyEngagedWith ActivePlayer) $ response $ entersPlay a]

instance RunMessage SarnFordSentry where
  runMessage msg s@(SarnFordSentry a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid =<< selectCount (EnemyEngagedWith $ PlayerWithId pid)
      pure s
    _ -> SarnFordSentry <$> runMessage msg a
