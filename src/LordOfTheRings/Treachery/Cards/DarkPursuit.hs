module LordOfTheRings.Treachery.Cards.DarkPursuit (darkPursuit) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Trait (Trait (Scout))

newtype DarkPursuit = DarkPursuit TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPursuit :: TreacheryCard DarkPursuit
darkPursuit = treachery DarkPursuit Cards.darkPursuit

instance RunMessage DarkPursuit where
  runMessage msg t@(DarkPursuit attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      scouts <- length <$> select (EnemyWithTrait Scout)
      if scouts == 0
        then phaseModifier attrs attrs (GainKeyword Surge)
        else phaseModifier attrs GameTarget (StagingThreatBonus scouts)
      pure t
    _ -> DarkPursuit <$> runMessage msg attrs
