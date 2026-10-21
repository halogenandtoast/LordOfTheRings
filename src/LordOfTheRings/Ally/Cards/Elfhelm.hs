module LordOfTheRings.Ally.Cards.Elfhelm (elfhelm) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Elfhelm = Elfhelm AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

elfhelm :: AllyCard Elfhelm
elfhelm = ally Elfhelm Cards.elfhelm

-- | While Elfhelm is ready, he gains: "Response: After your threat is raised as the result
-- of questing unsuccessfully, or by an encounter or quest card effect, reduce your threat
-- by 1."
instance HasAbilities Elfhelm where
  getAbilities (Elfhelm a) =
    restricted a 1 ready' (response $ QuestedUnsuccessfullyWindow After)
      : [restricted a 2 ready' $ response $ ThreatRaisedByEncounterWindow After pid | Just pid <- [placementController a.placement]]
   where
    ready' = exists $ AllyWithId a.id <> ReadyAlly

instance RunMessage Elfhelm where
  runMessage msg s@(Elfhelm a) = case msg of
    UseCardAbility pid (isSource a -> True) _ _ -> do
      lowerThreat pid 1
      pure s
    _ -> Elfhelm <$> runMessage msg a
