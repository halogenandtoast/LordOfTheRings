module LordOfTheRings.Ally.Cards.NaithGuide (naithGuide) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype NaithGuide = NaithGuide AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

naithGuide :: AllyCard NaithGuide
naithGuide = ally NaithGuide Cards.naithGuide

-- | Response: After Naith Guide enters play, choose a hero. That hero does not exhaust to
-- quest this round.
instance HasAbilities NaithGuide where
  getAbilities (NaithGuide a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage NaithGuide where
  runMessage msg s@(NaithGuide a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOneM pid do
        questionLabeled "chooseHero"
        causedBy a
        targets heroes \h -> roundModifier a h DoesNotExhaustToQuest
      pure s
    _ -> NaithGuide <$> runMessage msg a
