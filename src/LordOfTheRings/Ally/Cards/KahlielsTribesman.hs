module LordOfTheRings.Ally.Cards.KahlielsTribesman (kahlielsTribesman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Harad))

newtype KahlielsTribesman = KahlielsTribesman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kahlielsTribesman :: AllyCard KahlielsTribesman
kahlielsTribesman = ally KahlielsTribesman Cards.kahlielsTribesman

others :: AllyAttrs -> (HeroMatcher, AllyMatcher)
others a = (HeroWithTrait Harad, AllyWithTrait Harad <> not_ (AllyWithId a.id))

-- | Action: Exhaust Kahliel's Tribesman to choose another Harad character. That character
-- gets +1 willpower, +1 attack, and +1 defense until the end of the phase.
instance HasAbilities KahlielsTribesman where
  getAbilities (KahlielsTribesman a) =
    [restricted a 1 (uncurry (CharactersAtLeast 1) (others a)) $ ActionAbility (exhaustCost a)]

instance RunMessage KahlielsTribesman where
  runMessage msg s@(KahlielsTribesman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      harad <- uncurry selectCharacters (others a)
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy a
        targets harad \c -> phaseModifiers a c [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure s
    _ -> KahlielsTribesman <$> runMessage msg a
