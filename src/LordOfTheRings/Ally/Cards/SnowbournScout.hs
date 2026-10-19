module LordOfTheRings.Ally.Cards.SnowbournScout (snowbournScout) where

import LordOfTheRings.Ability
import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Matcher
import LordOfTheRings.Token

newtype SnowbournScout = SnowbournScout AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowbournScout :: AllyCard SnowbournScout
snowbournScout = ally SnowbournScout Cards.snowbournScout

instance HasAbilities SnowbournScout where
  getAbilities (SnowbournScout a) =
    [ response
        a
        1
        (after $ entersPlay a)
        (exists AnyLocation)
        "_Response_: After Snowbourn Scout enters play, choose a location. Place 1 progress token on that location."
    ]

instance RunMessage SnowbournScout where
  runMessage msg s@(SnowbournScout a) = case msg of
    UseCardAbility p (isSource a -> True) 1 -> do
      locations <- select AnyLocation
      push
        $ chooseOrRunOne p [targetLabel location [PlaceTokens (toSource a) (toTarget location) Progress 1] | location <- locations]
      pure s
    _ -> SnowbournScout <$> runMessage msg a
