module LordOfTheRings.Treachery.Cards.TheGuardiansFury (theGuardiansFury) where

import LordOfTheRings.Card (Card (EncounterCard), HasCardDef (..), cdCardTraits)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (revealFromVictoryDisplay)
import LordOfTheRings.Trait (Trait (Guardian))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheGuardiansFury = TheGuardiansFury TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGuardiansFury :: TreacheryCard TheGuardiansFury
theGuardiansFury = treachery TheGuardiansFury Cards.theGuardiansFury

-- | When Revealed: Each Guardian enemy makes an attack. If there are no Guardian enemies in
-- play, reveal a random Guardian enemy from the victory display, and add it to the staging
-- area.
-- Shadow: If the attacking enemy is a Guardian it makes an additional attack after this
-- one.
-- A Guardian attacks the player it is engaged with: every stage that has Guardians in play
-- has them engage a player.
instance RunMessage TheGuardiansFury where
  runMessage msg t@(TheGuardiansFury attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      guardians <- select (EnemyWithTrait Guardian)
      if null guardians
        then do
          victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
          chosen <- headMay <$> shuffleM [c | EncounterCard c <- victory, Guardian `member` cdCardTraits (toCardDef c)]
          traverse_ revealFromVictoryDisplay chosen
        else for_ guardians \eid -> do
          e <- getEnemy eid
          for_ (placementController e.placement) \pid -> immediateAttack pid eid
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      guardian <- selectAny (EnemyWithId eid <> EnemyWithTrait Guardian)
      when guardian do
        again <- capture $ immediateAttack pid eid
        afterAttack eid again
      pure t
    _ -> TheGuardiansFury <$> runMessage msg attrs
