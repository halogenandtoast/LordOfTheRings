module LordOfTheRings.Treachery.Cards.FireAndFoulFumes (fireAndFoulFumes) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationThreat)

newtype FireAndFoulFumes = FireAndFoulFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fireAndFoulFumes :: TreacheryCard FireAndFoulFumes
fireAndFoulFumes = treachery FireAndFoulFumes Cards.fireAndFoulFumes

instance RunMessage FireAndFoulFumes where
  runMessage msg t@(FireAndFoulFumes a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      locations <- select IsActiveLocation
      if null locations
        then revealEncounterCard
        else do
          n <- sum <$> traverse getLocationThreat locations
          eachPlayer \pid -> do
            characters <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId pid)) (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
            dealDamageDividedAmong a pid characters n
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      n <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
      characters <- charactersControlledBy pid
      dealDamageDividedAmong a pid characters n
      pure t
    _ -> FireAndFoulFumes <$> runMessage msg a
