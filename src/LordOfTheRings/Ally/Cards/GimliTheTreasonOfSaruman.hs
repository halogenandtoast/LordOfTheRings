module LordOfTheRings.Ally.Cards.GimliTheTreasonOfSaruman (gimliTheTreasonOfSaruman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GimliTheTreasonOfSaruman = GimliTheTreasonOfSaruman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
gimliTheTreasonOfSaruman :: AllyCard GimliTheTreasonOfSaruman
gimliTheTreasonOfSaruman = ally GimliTheTreasonOfSaruman Cards.gimliTheTreasonOfSaruman

-- | Response: After an enemy is revealed from the encounter deck, ready Gimli.
instance HasAbilities GimliTheTreasonOfSaruman where
  getAbilities (GimliTheTreasonOfSaruman a) =
    [restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly) $ response $ EnemyRevealedWindow After]

instance RunMessage GimliTheTreasonOfSaruman where
  runMessage msg s@(GimliTheTreasonOfSaruman a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> GimliTheTreasonOfSaruman <$> runMessage msg a
