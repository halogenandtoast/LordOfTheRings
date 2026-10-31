module LordOfTheRings.Enemy.Cards.RecurringNightmare (recurringNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RecurringNightmare = RecurringNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Dream-chaser campaign: a secret under a Temple of the Deceived, then
in the Campaign Pool for the rest of the campaign.
When Recurring Nightmare leaves play, shuffle it into any player's deck. Forced: When
Recurring Nightmare enters your hand, add it to the staging area.
ponytail: the engine keeps no encounter card in a player's deck: leaving play it is
discarded, and in the scenarios after it is shuffled into the encounter deck, as the
Campaign Pool's encounter cards are, rather than its player's deck.
-}
recurringNightmare :: EnemyCard RecurringNightmare
recurringNightmare = enemy RecurringNightmare Cards.recurringNightmare

-- | X is the number of players in the game: its defense.
instance HasModifiersFor RecurringNightmare where
  getModifiersFor (RecurringNightmare a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [DefenseModifier players]
