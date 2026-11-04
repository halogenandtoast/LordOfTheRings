module LordOfTheRings.Enemy.Cards.RecurringNightmare (recurringNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RecurringNightmare = RecurringNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Dream-chaser campaign: a secret under a Temple of the Deceived, then
in the Campaign Pool and its chosen player's deck for the rest of the campaign.
-}
recurringNightmare :: EnemyCard RecurringNightmare
recurringNightmare = enemy RecurringNightmare Cards.recurringNightmare

-- | X is the number of players in the game: its defense. When Recurring Nightmare leaves
-- play, shuffle it into any player's deck.
instance HasModifiersFor RecurringNightmare where
  getModifiersFor (RecurringNightmare a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [DefenseModifier players, LeavesPlayToAnyPlayersDeck]

-- | Forced: When Recurring Nightmare enters your hand, add it to the staging area.
instance RunMessage RecurringNightmare where
  runMessage msg e@(RecurringNightmare attrs) = case msg of
    EncounterCardEnteredHand pid card | card.id == attrs.cardId -> do
      removeFromHand pid card
      addToStaging card
      pure e
    _ -> RecurringNightmare <$> runMessage msg attrs
