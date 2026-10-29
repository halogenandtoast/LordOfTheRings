module LordOfTheRings.Ally.Cards.GreyfloodWanderer (greyfloodWanderer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Placement (Placement (Unplaced))

newtype GreyfloodWanderer = GreyfloodWanderer AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

greyfloodWanderer :: AllyCard GreyfloodWanderer
greyfloodWanderer = ally GreyfloodWanderer Cards.greyfloodWanderer

-- | You may give it doomed 2 when you play it from your hand. Asked while it is paid for.
instance HasModifiersFor GreyfloodWanderer where
  getModifiersFor (GreyfloodWanderer a) = when (a.placement == Unplaced) $ modified a (CardIdTarget a.allyCardId) [MayGainDoomed 2]

-- | If you do, it gains: "Response: After you play Greyflood Wanderer, place 1 progress token on each location in play."
instance HasAbilities GreyfloodWanderer where
  getAbilities (GreyfloodWanderer a) =
    [ restricted a 1 (TargetHasModifier (CardIdTarget a.allyCardId) (GainKeyword (Doomed 2)))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage GreyfloodWanderer where
  runMessage msg s@(GreyfloodWanderer a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      for_ locations \l -> placeProgress a l 1
      pure s
    _ -> GreyfloodWanderer <$> runMessage msg a
