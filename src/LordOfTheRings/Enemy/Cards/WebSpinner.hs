module LordOfTheRings.Enemy.Cards.WebSpinner (webSpinner) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype WebSpinner = WebSpinner EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

webSpinner :: EnemyCard WebSpinner
webSpinner = enemy WebSpinner Cards.webSpinner

-- | When Revealed: The player guarding Gollum must either exhaust a hero he controls, or
-- Web-spinner gains surge.
-- Shadow: the player guarding Gollum must exhaust a character he controls.
instance RunMessage WebSpinner where
  runMessage msg e@(WebSpinner attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getGuard >>= \case
        Nothing -> revealEncounterCard
        Just guard' -> do
          heroes <- select (HeroControlledBy (PlayerWithId guard') <> ReadyHero)
          chooseOneM guard' do
            questionLabeled "webSpinner.choose"
            causedBy attrs
            targets heroes exhaust
            labeled "webSpinner.surge" revealEncounterCard
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      getGuard >>= traverse_ \guard' -> chooseAndExhaust attrs guard' 1
      pure e
    _ -> WebSpinner <$> runMessage msg attrs
