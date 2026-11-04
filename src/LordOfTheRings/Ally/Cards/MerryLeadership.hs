module LordOfTheRings.Ally.Cards.MerryLeadership (merryLeadership) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype MerryLeadership = MerryLeadership AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

merryLeadership :: AllyCard MerryLeadership
merryLeadership = ally MerryLeadership Cards.merryLeadership

-- | Response: After Merry enters play, each unique ally you control gets +1 willpower
-- until the end of the round.
--
-- The allies are those you control as the response resolves, Merry among them: a player
-- card's lasting effect is not recalculated (FAQ 1.55).
instance HasAbilities MerryLeadership where
  getAbilities (MerryLeadership a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage MerryLeadership where
  runMessage msg s@(MerryLeadership a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> UniqueAlly)
      for_ allies \x -> roundModifier a x (WillpowerModifier 1)
      pure s
    _ -> MerryLeadership <$> runMessage msg a
