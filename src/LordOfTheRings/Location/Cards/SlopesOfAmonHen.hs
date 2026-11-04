module LordOfTheRings.Location.Cards.SlopesOfAmonHen (slopesOfAmonHen) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (attacksTheWeaker, enemiesInAnyStagingArea, playersAtThisStage)

newtype SlopesOfAmonHen = SlopesOfAmonHen LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

slopesOfAmonHen :: LocationCard SlopesOfAmonHen
slopesOfAmonHen = location SlopesOfAmonHen Cards.slopesOfAmonHen

-- | The players cannot travel here with no enemy in any staging area for one of them to
-- engage. One in the staging area in focus may be barred from engaging them, as The
-- Great River's enemies are.
engageable :: HasGame m => m [(PlayerId, EnemyId)]
engageable = do
  players <- playersAtThisStage
  enemies <- enemiesInAnyStagingArea
  flip filterM [(p, e) | p <- players, e <- enemies] \(p, e) -> notElem (CannotEngagePlayer p) <$> getModifiers e

instance HasModifiersFor SlopesOfAmonHen where
  getModifiersFor (SlopesOfAmonHen a) = do
    none <- null <$> engageable
    when none $ modifySelf a [CannotBeTraveledTo]

-- | Travel: A player at this staging area must engage an enemy in any staging area to travel
-- here. An enemy in another staging area comes to this one as it is engaged.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character has
-- fewer printed hit points than the attacking enemy).
instance RunMessage SlopesOfAmonHen where
  runMessage msg (SlopesOfAmonHen a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getStageLead
      candidates <- engageable
      chooseOrRunOneM lead do
        questionLabeled "slopesOfAmonHen.choosePlayer"
        causedBy a
        targets (ordNub $ map fst candidates) \p -> chooseOrRunOneM p do
          questionLabeled "slopesOfAmonHen.engage"
          causedBy a
          targets [e | (q, e) <- candidates, q == p] \e -> do
            placeInStagingAreaOf p e
            engageEnemy p e
      SlopesOfAmonHen <$> runMessage msg a
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      attacksTheWeaker (CardIdSource cid) eid defenders
      pure $ SlopesOfAmonHen a
    _ -> SlopesOfAmonHen <$> runMessage msg a
