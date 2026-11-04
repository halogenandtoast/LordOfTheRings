module LordOfTheRings.Ally.Cards.Meneldor (meneldor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Meneldor = Meneldor AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

meneldor :: AllyCard Meneldor
meneldor = ally Meneldor Cards.meneldor

-- | Cannot have restricted attachments.
instance HasModifiersFor Meneldor where
  getModifiersFor (Meneldor a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Response: After Meneldor enters or leaves play, place 2 progress on a location.
instance HasAbilities Meneldor where
  getAbilities (Meneldor a) =
    [ restricted a 1 (exists AnyLocation) $ response $ entersPlay a
    , restricted a 2 (exists AnyLocation) $ response $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage Meneldor where
  runMessage msg s@(Meneldor a) = case msg of
    UseCardAbility pid (isSource a -> True) n _ | n `elem` [1, 2] -> do
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "meneldor.location"
        causedBy a
        targets locations \l -> placeProgress a l 2
      pure s
    _ -> Meneldor <$> runMessage msg a
