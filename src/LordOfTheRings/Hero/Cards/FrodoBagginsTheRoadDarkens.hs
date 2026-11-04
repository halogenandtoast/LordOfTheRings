module LordOfTheRings.Hero.Cards.FrodoBagginsTheRoadDarkens (frodoBagginsTheRoadDarkens) where

import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Helpers.Fellowship (exhaustTheOneRing, sagaOneRingReady)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype FrodoBagginsTheRoadDarkens = FrodoBagginsTheRoadDarkens HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheRoadDarkens :: HeroCard FrodoBagginsTheRoadDarkens
frodoBagginsTheRoadDarkens = hero FrodoBagginsTheRoadDarkens Cards.frodoBagginsTheRoadDarkens

-- | Response: After Frodo Baggins exhausts to defend an attack, exhaust The One Ring and
-- spend 1 [fellowship] resource to target the attacking enemy. Then, this attack deals no
-- damage and each player raises his threat by 2. Exhausted as he is declared, he exhausted
-- to defend; one that defends without exhausting does not.
instance HasAbilities FrodoBagginsTheRoadDarkens where
  getAbilities (FrodoBagginsTheRoadDarkens a) =
    [ restricted a 1 (sagaOneRingReady <> exists (HeroWithId a.id <> ExhaustedHero))
        $ ResponseAbility (DefenderDeclaredWindow After (Just $ toTarget a)) (SphereResourceCost a.owner Fellowship 1)
    ]

instance RunMessage FrodoBagginsTheRoadDarkens where
  runMessage msg h@(FrodoBagginsTheRoadDarkens attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      exhaustTheOneRing
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> do
        -- an enemy immune to player card effects cannot be targeted
        unlessM (immuneToSource (toSource attrs) eid) do
          createEffect attrs eid [AttackDealsNoDamage] UntilEndOfAttack
          eachPlayer \p -> raiseThreat p 2
      pure h
    _ -> FrodoBagginsTheRoadDarkens <$> runMessage msg attrs
