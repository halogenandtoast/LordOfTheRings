module LordOfTheRings.Ally.Cards.Bombur (bombur) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Underground))

newtype Bombur = Bombur AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bombur :: AllyCard Bombur
bombur = ally Bombur Cards.bombur

-- | Action: Exhaust Bombur to choose a location. That location gets -1 threat until the
-- end of the phase. (That location does not contribute its threat instead if it is an
-- Underground location.)
instance HasAbilities Bombur where
  getAbilities (Bombur a) = [restricted a 1 (exists AnyLocation) $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Bombur where
  runMessage msg s@(Bombur a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      chooseOneM pid do
        questionLabeled "bombur.chooseLocation"
        causedBy a
        targets locations \l -> do
          underground <- selectAny (LocationWithId l <> LocationWithTrait Underground)
          phaseModifier a l $ if underground then DoesNotContributeThreat else ThreatModifier (-1)
      pure s
    _ -> Bombur <$> runMessage msg a
