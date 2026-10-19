module LordOfTheRings.Ally.Cards.HenamarthRiversong (henamarthRiversong) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HenamarthRiversong = HenamarthRiversong AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

henamarthRiversong :: AllyCard HenamarthRiversong
henamarthRiversong = ally HenamarthRiversong Cards.henamarthRiversong

-- | Action: Exhaust Henamarth Riversong to look at the top card of the encounter deck.
instance HasAbilities HenamarthRiversong where
  getAbilities (HenamarthRiversong a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage HenamarthRiversong where
  runMessage msg s@(HenamarthRiversong a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtTopOfEncounterDeck pid a False
      pure s
    _ -> HenamarthRiversong <$> runMessage msg a
