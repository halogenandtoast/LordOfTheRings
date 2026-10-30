module LordOfTheRings.Ally.Cards.Deorwine (deorwine) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype Deorwine = Deorwine AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on his card def.
deorwine :: AllyCard Deorwine
deorwine = ally Deorwine Cards.deorwine

-- | While Déorwine is defending, he gains: "Response: Spend 1 leadership resource to cancel
-- a shadow effect just triggered. Any player may trigger this response."
instance HasAbilities Deorwine where
  getAbilities (Deorwine a) =
    [ anyPlayer
        $ restricted a 1 EncounterEffectsCancelable
        $ ResponseAbility (ShadowEffectAgainstWindow When (toTarget a)) (SpendResourcesCost Leadership 1)
    ]

instance RunMessage Deorwine where
  runMessage msg s@(Deorwine a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      cancelShadowEffects windows
      pure s
    _ -> Deorwine <$> runMessage msg a
