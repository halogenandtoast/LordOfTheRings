module LordOfTheRings.Treachery.Cards.TheEnemysTrail (theEnemysTrail) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers (clueToMordorEnemy, mordorEnemies)
import LordOfTheRings.Trait (Trait (Clue))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheEnemysTrail = TheEnemysTrail TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEnemysTrail :: TreacheryCard TheEnemysTrail
theEnemysTrail = treachery TheEnemysTrail Cards.theEnemysTrail

-- | When Revealed: Search the encounter discard pile for a Clue card and attach it to a
-- Mordor enemy in play, if able. If no card is attached by this effect, The Enemy's Trail
-- gains surge.
-- Shadow: Search the encounter discard pile for a Clue card and attach it to a Mordor enemy
-- in play, if able.
instance RunMessage TheEnemysTrail where
  runMessage msg t@(TheEnemysTrail attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attached <- trail
      unless attached revealEncounterCard
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      _ <- trail
      pure t
    _ -> TheEnemysTrail <$> runMessage msg attrs
   where
    trail = do
      clues <- filter ((Clue `member`) . cdCardTraits . toCardDef) <$> getEncounterDiscard
      enemies <- mordorEnemies
      lead <- getLead
      case clues of
        clue : _ | notNull enemies -> clueToMordorEnemy attrs lead clue $> True
        _ -> pure False
