module LordOfTheRings.Treachery.Cards.TheDragonsIre (theDragonsIre) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDragonsIre = TheDragonsIre TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDragonsIre :: TreacheryCard TheDragonsIre
theDragonsIre = treachery TheDragonsIre Cards.theDragonsIre

instance RunMessage TheDragonsIre where
  runMessage msg t@(TheDragonsIre a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      players <- getRemainingPlayers
      dragons <- select (EnemyWithTrait Dragon)
      next (0 :: Int) [(pid, eid) | pid <- players, eid <- dragons]
      pure t
    ScenarioSpecific "theDragonsIre.attacks" (parseMaybe parseJSON -> Just (tid, total, attacks)) | tid == a.id -> do
      case attacks of
        [] -> do
          dragons <- select (EnemyWithTrait Dragon)
          when (total > 0) $ for_ dragons \eid -> placeProgress a eid total
        (pid, eid) : _ -> do
          immediateAttackWithoutShadow pid eid
          doStep 1 (WithTarget (EnemyTarget eid) msg)
      pure t
    DoStep
      1
      ( WithTarget
          (EnemyTarget eid)
          (ScenarioSpecific "theDragonsIre.attacks" (parseMaybe parseJSON -> Just (tid, total, (_ : rest))))
        ) | tid == a.id -> do
      destroyed <- attackDestroyed eid
      next (total + length destroyed) rest
      pure t
    _ -> TheDragonsIre <$> runMessage msg a
   where
    -- Count each attack before the same Dragon's next attack clears its record.
    -- The second sentence places progress only after all attacks have resolved.
    next total attacks = push $ ScenarioSpecific "theDragonsIre.attacks" (toJSON (a.id, total, attacks))
