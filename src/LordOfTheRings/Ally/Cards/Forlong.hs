module LordOfTheRings.Ally.Cards.Forlong (forlong) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdSphere, toCardDef)
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait (Trait (Outlands))

newtype Forlong = Forlong AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forlong :: AllyCard Forlong
forlong = ally Forlong Cards.forlong

-- | While you control Outlands allies that belong to 4 different spheres, ready Forlong
-- at the beginning of each phase.
instance RunMessage Forlong where
  runMessage msg (Forlong a) = case msg of
    BeginPhase _ | Just pid <- placementController a.placement -> do
      allies <- traverse getAlly =<< select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Outlands)
      let spheres = ordNub [s' | Just s' <- map (cdSphere . toCardDef) allies, s' /= Neutral]
      when (length spheres >= 4) $ ready a
      Forlong <$> runMessage msg a
    _ -> Forlong <$> runMessage msg a
