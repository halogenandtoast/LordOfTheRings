module LordOfTheRings.Treachery.Cards.CrawlingTowardsHim (crawlingTowardsHim) where

import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisPhase)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CrawlingTowardsHim = CrawlingTowardsHim TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Hide 2 (the game makes the test before the When Revealed effect).
crawlingTowardsHim :: TreacheryCard CrawlingTowardsHim
crawlingTowardsHim = treachery CrawlingTowardsHim Cards.crawlingTowardsHim

-- | When Revealed: If you have failed a Hide test this phase, remove each character you
-- control from the quest.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage CrawlingTowardsHim where
  runMessage msg t@(CrawlingTowardsHim attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      whenM (failedHideTestThisPhase you) do
        committed <- selectCharacters (HeroControlledBy (PlayerWithId you) <> CommittedHero) (AllyControlledBy (PlayerWithId you) <> CommittedAlly)
        for_ committed removeFromQuest
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> CrawlingTowardsHim <$> runMessage msg attrs
