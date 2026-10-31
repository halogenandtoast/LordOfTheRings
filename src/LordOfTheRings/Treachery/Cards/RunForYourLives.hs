module LordOfTheRings.Treachery.Cards.RunForYourLives (runForYourLives) where

import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, discardRescuedVillagers, rescuedVillagerOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RunForYourLives = RunForYourLives TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

runForYourLives :: TreacheryCard RunForYourLives
runForYourLives = treachery RunForYourLives Cards.runForYourLives

-- | When Revealed: Discard 1 villager token from Rescued Villagers. Assign X damage among
-- characters in play. X is the number of damage tokens on Dead Villagers: counted once the
-- villager is discarded, the first player assigning it.
-- Shadow: Either discard 1 villager token from Rescued Villagers, or this attack is
-- considered undefended. An undefended attack already is, so it asks nothing.
instance RunMessage RunForYourLives where
  runMessage msg t@(RunForYourLives attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardRescuedVillagers attrs 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- deadVillagerCount
      characters <- selectCharacters AnyHero AnyAlly
      lead <- getLead
      when (x > 0) $ dealDamageDividedAmong attrs lead characters x
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      unless (null defenders) $ rescuedVillagerOr attrs pid "runForYourLives.undefended" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure t
    _ -> RunForYourLives <$> runMessage msg attrs
