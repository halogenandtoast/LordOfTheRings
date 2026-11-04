module LordOfTheRings.Ally.Cards.StewardOfOrthanc (stewardOfOrthanc) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (placementController)

newtype StewardOfOrthanc = StewardOfOrthanc AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stewardOfOrthanc :: AllyCard StewardOfOrthanc
stewardOfOrthanc = ally StewardOfOrthanc Cards.stewardOfOrthanc

-- | Marks the player who triggered a Steward of Orthanc this round, any copy of it.
triggered :: ModifierType
triggered = UsedThisRound (toCardCode Cards.stewardOfOrthanc)

-- | Response: When you play an event, give it Doomed 1 to draw a card. You cannot trigger
-- this ability if you have already triggered the ability of a Steward of Orthanc this
-- round. The event's Doomed 1 is resolved at once: each player raises his threat by 1.
instance HasAbilities StewardOfOrthanc where
  getAbilities (StewardOfOrthanc a) =
    [ restricted a 1 (TargetLacksModifier (PlayerTarget pid) triggered)
        $ response
        $ PlayerPlayingEventWindow When pid
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage StewardOfOrthanc where
  runMessage msg s@(StewardOfOrthanc a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      roundModifier a (PlayerTarget pid) triggered
      raiseThreatFromDoomed 1
      drawCards a pid 1
      pure s
    _ -> StewardOfOrthanc <$> runMessage msg a
