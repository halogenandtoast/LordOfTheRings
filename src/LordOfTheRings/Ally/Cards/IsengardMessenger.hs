module LordOfTheRings.Ally.Cards.IsengardMessenger (isengardMessenger) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype IsengardMessenger = IsengardMessenger AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

isengardMessenger :: AllyCard IsengardMessenger
isengardMessenger = ally IsengardMessenger Cards.isengardMessenger

-- | Response: After you raise your threat from the Doomed keyword, Isengard Messenger gets
-- +1 willpower until the end of the round. (Limit twice per round.)
instance HasAbilities IsengardMessenger where
  getAbilities (IsengardMessenger a) =
    [ limited (PlayerLimit PerRound 2) $ mkAbility a 1 $ response $ ThreatRaisedByDoomedWindow After pid
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage IsengardMessenger where
  runMessage msg s@(IsengardMessenger a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      roundModifier a a (WillpowerModifier 1)
      pure s
    _ -> IsengardMessenger <$> runMessage msg a
