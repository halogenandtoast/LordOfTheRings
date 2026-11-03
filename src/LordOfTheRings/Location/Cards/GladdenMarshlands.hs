module LordOfTheRings.Location.Cards.GladdenMarshlands (gladdenMarshlands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GladdenMarshlands = GladdenMarshlands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gladdenMarshlands :: LocationCard GladdenMarshlands
gladdenMarshlands = location GladdenMarshlands Cards.gladdenMarshlands

-- | Action: Deal 1 damage to a hero you control to reduce Gladden Marshlands' threat by 1
-- until the end of the phase. Any player may trigger this effect.
instance HasAbilities GladdenMarshlands where
  getAbilities (GladdenMarshlands a) = [anyPlayer $ mkAbility a 1 $ ActionAbility Free]

-- | Shadow: Each enemy engaged with the defending character gets +1 attack until the end of
-- the phase. (+2 attack instead if this attack is undefended.)
instance RunMessage GladdenMarshlands where
  runMessage msg l@(GladdenMarshlands a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "gladdenMarshlands.damage"
        causedBy a
        targets heroes \h -> do
          dealDamage a h 1
          phaseModifier a a (ThreatModifier (-1))
      pure l
    ResolveShadowEffect cid _ pid defender | cid == a.cardId -> do
      engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
      for_ engaged \e -> phaseModifier a e (AttackModifier $ ifUndefended defender 2 1)
      pure l
    _ -> GladdenMarshlands <$> runMessage msg a
