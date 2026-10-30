module LordOfTheRings.Treachery.Cards.SunkenRuins (sunkenRuins) where

import LordOfTheRings.Trait (Trait (Ruins))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SunkenRuins = SunkenRuins TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sunkenRuins :: TreacheryCard SunkenRuins
sunkenRuins = treachery SunkenRuins Cards.sunkenRuins

-- | Doomed 1.
-- When Revealed: Each Uncharted location in the staging area gets +1 threat until the end
-- of the phase. If the active location has the Ruins trait, discard 1 resource from each
-- hero and objective ally in play.
instance RunMessage SunkenRuins where
  runMessage msg t@(SunkenRuins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      islands <- select (LocationInStagingArea <> unchartedLocation)
      for_ islands \l -> phaseModifier attrs l (ThreatModifier 1)
      ruins <- selectAny (IsActiveLocation <> LocationWithTrait Ruins)
      when ruins do
        heroes <- select HeroWithResources
        objectiveAllies <- select ObjectiveAlly
        for_ heroes \h -> removeTokens attrs h Resource 1
        for_ objectiveAllies \a -> removeTokens attrs a Resource 1
      pure t
    _ -> SunkenRuins <$> runMessage msg attrs
