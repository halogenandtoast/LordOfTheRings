module LordOfTheRings.Enemy.Cards.ThrongsOfUnfaithful (throngsOfUnfaithful) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ThrongsOfUnfaithful = ThrongsOfUnfaithful EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

throngsOfUnfaithful :: EnemyCard ThrongsOfUnfaithful
throngsOfUnfaithful = enemy ThrongsOfUnfaithful Cards.throngsOfUnfaithful

-- | Surge.
-- Forced: When Throngs of Unfaithful is destroyed, discard the bottom card of the engaged
-- player's deck. If that card's printed cost is 2 or less, return Throngs of Unfaithful to
-- the top of the encounter deck.
instance HasAbilities ThrongsOfUnfaithful where
  getAbilities (ThrongsOfUnfaithful a) = [mkAbility a 1 $ forced $ ThisEnemyDefeatedWindow When a.id]

instance RunMessage ThrongsOfUnfaithful where
  runMessage msg e@(ThrongsOfUnfaithful attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case attrs.placement of
        EngagedWith pid -> do
          bottom <- discardBottomOfDeck pid
          when (maybe False ((<= 2) . printedCostOf) bottom) $ returnToTopOfEncounterDeck attrs
        _ -> pure ()
      pure e
    _ -> ThrongsOfUnfaithful <$> runMessage msg attrs
