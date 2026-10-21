module LordOfTheRings.Ally.Cards.SilvanRefugee (silvanRefugee) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SilvanRefugee = SilvanRefugee AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silvanRefugee :: AllyCard SilvanRefugee
silvanRefugee = ally SilvanRefugee Cards.silvanRefugee

-- | Forced: After a character leaves play, discard Silvan Refugee from play.
instance HasAbilities SilvanRefugee where
  getAbilities (SilvanRefugee a) = [mkAbility a 1 $ forced $ CharacterLeftPlayWindow After]

instance RunMessage SilvanRefugee where
  runMessage msg s@(SilvanRefugee a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      pure s
    _ -> SilvanRefugee <$> runMessage msg a
