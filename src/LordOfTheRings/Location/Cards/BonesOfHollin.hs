module LordOfTheRings.Location.Cards.BonesOfHollin (bonesOfHollin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (OstInEdhil))

newtype BonesOfHollin = BonesOfHollin LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bonesOfHollin :: LocationCard BonesOfHollin
bonesOfHollin = location BonesOfHollin Cards.bonesOfHollin

-- | Forced: After any amount of damage is placed on Bones of Hollin, raise each player's
-- threat by that amount. Read as the damage is placed.
-- Travel: Place 2 damage on an Ost-in-Edhil location in play to travel here.
instance RunMessage BonesOfHollin where
  runMessage msg l@(BonesOfHollin a) = case msg of
    PlaceTokens _ (isTarget a -> True) Damage n | n > 0 -> do
      eachPlayer \p -> encounterRaisesThreat p n
      BonesOfHollin <$> runMessage msg a
    TravelTo lid | lid == a.id -> do
      ruins <- select (LocationWithTrait OstInEdhil)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "bonesOfHollin.damage"
        causedBy a
        targets ruins \x -> placeDamage a x 2
      pure l
    _ -> BonesOfHollin <$> runMessage msg a
