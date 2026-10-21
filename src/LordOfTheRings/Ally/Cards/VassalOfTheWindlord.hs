module LordOfTheRings.Ally.Cards.VassalOfTheWindlord (vassalOfTheWindlord) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype VassalOfTheWindlord = VassalOfTheWindlord AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vassalOfTheWindlord :: AllyCard VassalOfTheWindlord
vassalOfTheWindlord = ally VassalOfTheWindlord Cards.vassalOfTheWindlord

-- | Ranged. Vassal of the Windlord cannot have restricted attachments.
-- Forced: After an attack in which Vassal of the Windlord attacked resolves, discard
-- Vassal of the Windlord from play.
instance HasAbilities VassalOfTheWindlord where
  getAbilities (VassalOfTheWindlord a) = [mkAbility a 1 $ forced $ attacked a]

instance RunMessage VassalOfTheWindlord where
  runMessage msg s@(VassalOfTheWindlord a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      pure s
    _ -> VassalOfTheWindlord <$> runMessage msg a
