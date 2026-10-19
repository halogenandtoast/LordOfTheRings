module LordOfTheRings.Ally.Cards.SnowbournScout (snowbournScout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Matcher
import LordOfTheRings.Token

newtype SnowbournScout = SnowbournScout AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowbournScout :: AllyCard SnowbournScout
snowbournScout = ally SnowbournScout Cards.snowbournScout

instance HasAbilities SnowbournScout where
  getAbilities (SnowbournScout a) =
    [restricted a 1 (exists AnyLocation) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage SnowbournScout where
  runMessage msg s@(SnowbournScout a) = case msg of
    UseCardAbility p (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      chooseOrRunOneM p $ targets locations \location -> placeProgress a location 1
      pure s
    _ -> SnowbournScout <$> runMessage msg a
