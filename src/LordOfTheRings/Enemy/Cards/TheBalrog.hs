module LordOfTheRings.Enemy.Cards.TheBalrog (theBalrog) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype TheBalrog = TheBalrog EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBalrog :: EnemyCard TheBalrog
theBalrog = enemy TheBalrog Cards.theBalrog

{- | Indestructible. Cannot be optionally engaged. The Balrog and shadow cards dealt to The
Balrog are immune to player card effects (a player card canceling a shadow effect is one).
While in the staging area, The Balrog is considered to be engaged with the first player,
attacking him in the combat phase, and only the first player can declare attackers against
The Balrog.
-}
instance HasModifiersFor TheBalrog where
  getModifiersFor (TheBalrog a) = do
    modifySelf a [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer, EngagedWithEachPlayer FirstPlayer]
    for_ a.shadowCards \c -> modified a (CardIdTarget c.id) [ImmuneToPlayerCardEffects, ShadowCannotBeCanceled]
    when (a.placement == StagingArea) do
      lead <- getLead
      others <- filter (/= lead) <$> getRemainingPlayers
      modifySelf a [CannotBeAttackedByCharactersOf p | p <- others]
