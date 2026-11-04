module LordOfTheRings.Treachery.Cards.DarkCovenant (darkCovenant) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (progressOn)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkCovenant = DarkCovenant TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkCovenant :: TreacheryCard DarkCovenant
darkCovenant = treachery DarkCovenant Cards.darkCovenant

-- | When Revealed: Discard the top 3 cards of the encounter deck. The first player must
-- either remove X progress from among the quests in play, or assign X damage among
-- characters he controls. X is the total threat of the discarded cards.
-- Shadow: If this attack destroys a character, raise your threat by the destroyed
-- character's willpower.
instance RunMessage DarkCovenant where
  runMessage msg t@(DarkCovenant attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardTopOfEncounterDeck attrs 3
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let x = sum $ mapMaybe (cdThreat . toCardDef) cards
      lead <- getLead
      progress <- sum <$> (traverse progressOn =<< getQuestCards)
      when (x > 0) $ chooseOrRunOneM lead do
        questionLabeled "darkCovenant.choose"
        causedBy attrs
        when (progress >= x) $ labeled "darkCovenant.removeProgress" $ doStep x removing
        labeled "darkCovenant.assignDamage" $ dealDamageDivided attrs lead x
      pure t
    -- the first player removes the progress 1 at a time, from the quest cards he chooses
    DoStep n (WhenRevealed (isTarget attrs -> True)) | n > 0 -> do
      lead <- getLead
      quests <- filterM (fmap (> 0) . progressOn) =<< getQuestCards
      chooseOrRunOneM lead do
        questionLabeled "darkCovenant.removeProgressFrom"
        causedBy attrs
        targets quests \q -> do
          removeTokens attrs q Progress 1
          doStep (n - 1) removing
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      destroyed <- attackDestroyed eid
      for_ destroyed \d -> encounterRaisesThreat pid $ fromMaybe 0 $ cdWillpower $ toCardDef d.card
      pure t
    _ -> DarkCovenant <$> runMessage msg attrs
   where
    removing = WhenRevealed (toTarget attrs)
