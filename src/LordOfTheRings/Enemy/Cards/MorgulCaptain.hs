module LordOfTheRings.Enemy.Cards.MorgulCaptain (morgulCaptain) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Orc))

newtype MorgulCaptain = MorgulCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
morgulCaptain :: EnemyCard MorgulCaptain
morgulCaptain = enemy MorgulCaptain Cards.morgulCaptain

{- | When Revealed: Put the topmost Orc enemy in the discard pile into play engaged with you.
Shadow: If this attack destroys a character, return attacking enemy to the staging area.
-}
instance RunMessage MorgulCaptain where
  runMessage msg e@(MorgulCaptain attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      discarded <- getsGame (scenarioEncounterDiscard . toAttrs . (.scenario))
      for_ (find (cardMatches $ CardMatchAll [CardWithType EnemyType, CardWithTrait Orc]) discarded) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged you card
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      placeEnemy eid StagingArea
      pure e
    _ -> MorgulCaptain <$> runMessage msg attrs
