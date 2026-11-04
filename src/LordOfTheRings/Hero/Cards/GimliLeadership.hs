module LordOfTheRings.Hero.Cards.GimliLeadership (gimliLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype GimliLeadership = GimliLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, as its card def says.
gimliLeadership :: HeroCard GimliLeadership
gimliLeadership = hero GimliLeadership Cards.gimliLeadership

others :: HeroAttrs -> HeroMatcher
others a = ExhaustedHero <> not_ (HeroWithId a.id)

-- | Response: After Gimli is declared as a defender, spend 1 resource from his pool to ready
-- another hero. If that hero is Legolas, he gets +1 attack until the end of the phase.
-- (Limit once per phase.)
instance HasAbilities GimliLeadership where
  getAbilities (GimliLeadership a) =
    [ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (exists $ others a)
        $ ResponseAbility (DefenderDeclaredWindow After (Just $ toTarget a)) (ResourceCost (toTarget a) 1)
    ]

instance RunMessage GimliLeadership where
  runMessage msg h@(GimliLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (others attrs)
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroToReady"
        causedBy attrs
        targets heroes \other -> do
          ready other
          legolas <- selectAny (HeroWithId other <> HeroWithTitle "Legolas")
          when legolas $ phaseModifier attrs other (AttackModifier 1)
      pure h
    _ -> GimliLeadership <$> runMessage msg attrs
