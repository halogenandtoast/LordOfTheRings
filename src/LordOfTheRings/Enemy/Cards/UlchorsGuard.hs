module LordOfTheRings.Enemy.Cards.UlchorsGuard (ulchorsGuard) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

newtype UlchorsGuard = UlchorsGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ulchorsGuard :: EnemyCard UlchorsGuard
ulchorsGuard = enemy UlchorsGuard Cards.ulchorsGuard

-- | The player Thane Ulchor is engaged with, whichever version of him is in play.
thaneEngagedWith :: HasGame m => m (Maybe PlayerId)
thaneEngagedWith = do
  thanes <- traverse getEnemy =<< select thaneUlchor
  pure $ headMay [p | t <- thanes, EngagedWith p <- [t.placement]]

-- | While Thane Ulchor is engaged with a player, Ulchor's Guard engages that player: no
-- other player can engage it. ("Thane of Ulchor" on the card is Thane Ulchor.)
instance HasModifiersFor UlchorsGuard where
  getModifiersFor (UlchorsGuard a) =
    thaneEngagedWith >>= traverse_ \p -> do
      others <- filter (/= p) <$> getPlayerOrder
      modifySelf a $ EngagesOnly p : map CannotEngagePlayer others

{- | It engages Thane Ulchor's player as it enters play, as he engages a player, and
whenever it would return to the staging area while he is engaged.
Shadow: Attacking enemy gets +2 Defense until the end of the phase.
-}
instance RunMessage UlchorsGuard where
  runMessage msg e@(UlchorsGuard attrs) = case msg of
    CheckWindows ws | any following ws -> do
      thaneEngagedWith >>= traverse_ \p ->
        when (attrs.placement /= EngagedWith p) $ engageEnemy p attrs.id
      UlchorsGuard <$> runMessage msg attrs
    PlaceEnemy eid StagingArea | eid == attrs.id -> do
      thaneEngagedWith >>= \case
        Just p -> do
          engageEnemy p attrs.id
          pure e
        Nothing -> UlchorsGuard <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 2)
      pure e
    _ -> UlchorsGuard <$> runMessage msg attrs
   where
    following = \case
      Window After (EnemyEnteredPlay eid) -> eid == attrs.id
      Window After (EnemyEngaged _ _) -> True
      _ -> False
