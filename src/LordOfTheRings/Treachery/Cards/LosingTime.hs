module LordOfTheRings.Treachery.Cards.LosingTime (losingTime) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LosingTime = LosingTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Angmar Awakened campaign, from Across the Ettenmoors. Surge, on its
-- card def.
losingTime :: TreacheryCard LosingTime
losingTime = treachery LosingTime Cards.losingTime

-- | When Revealed: Each player must either raise their threat by 3 or exhaust a hero they
-- control.
instance RunMessage LosingTime where
  runMessage msg t@(LosingTime attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy (PlayerWithId p) <> ReadyHero)
        chooseOrRunOneM p do
          questionLabeled "losingTime.choose"
          causedBy attrs
          labeled "losingTime.raiseThreat" $ encounterRaisesThreat p 3
          targets heroes exhaust
      pure t
    _ -> LosingTime <$> runMessage msg attrs
