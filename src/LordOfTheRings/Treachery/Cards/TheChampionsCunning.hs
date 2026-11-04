module LordOfTheRings.Treachery.Cards.TheChampionsCunning (theChampionsCunning) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (getWainriderChampion, placeWainridersDamage)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheChampionsCunning = TheChampionsCunning TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theChampionsCunning :: TreacheryCard TheChampionsCunning
theChampionsCunning = treachery TheChampionsCunning Cards.theChampionsCunning

-- | When Revealed: Wainrider Champion makes an attack against each engaged player. If no
-- attacks were made, place damage on The Wainriders' stage equal to the Wainrider
-- Champion's Threat. The players engaged with it are each player while The Challengers is
-- at the same stage as The Wainriders.
-- Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, place 2
-- damage on The Wainriders' stage.
instance RunMessage TheChampionsCunning where
  runMessage msg t@(TheChampionsCunning attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getWainriderChampion >>= traverse_ \champion -> do
        engaged <- filterM (\p -> selectAny (EnemyWithId champion <> EnemyEngagedWith (PlayerWithId p))) =<< getRemainingPlayers
        if null engaged
          then placeWainridersDamage =<< getEnemyThreat champion
          else for_ engaged \p -> immediateAttack p champion
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeWainridersDamage 2
      pure t
    _ -> TheChampionsCunning <$> runMessage msg attrs
