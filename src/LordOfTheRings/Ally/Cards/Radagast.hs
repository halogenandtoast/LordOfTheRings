module LordOfTheRings.Ally.Cards.Radagast (radagast) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Trait (Trait (Creature))

newtype Radagast = Radagast AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

radagast :: AllyCard Radagast
radagast = ally Radagast Cards.radagast

-- | Radagast collects 1 resource each resource phase. These resources can be used to pay
-- for Creature cards played from your hand.
instance HasModifiersFor Radagast where
  getModifiersFor (Radagast a) = modified a a [ResourcesPayFor (CardWithTrait Creature)]

-- | Action: Spend X resources from Radagast's pool to heal X wounds on any 1 Creature.
instance HasAbilities Radagast where
  getAbilities (Radagast a) = [mkAbility a 1 $ ActionAbility Free | countTokens Resource a.allyTokens > 0]

instance RunMessage Radagast where
  runMessage msg s@(Radagast a) = case msg of
    BeginPhase ResourcePhase -> do
      placeResources a a 1
      Radagast <$> runMessage msg a
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      creatures <- damagedCharacters (HeroWithTrait Creature) (AllyWithTrait Creature)
      chooseOneM pid do
        questionLabeled "radagast.heal"
        causedBy a
        targets creatures \c -> chooseOneM pid do
          questionLabeled "chooseX"
          causedBy a
          for_ [1 .. countTokens Resource a.allyTokens] \x -> labeledWith "xEquals" [intVar "x" x] do
            removeTokens a a Resource x
            healDamage a c x
      pure s
    _ -> Radagast <$> runMessage msg a
