module LordOfTheRings.Treachery.Cards.SaveUs (saveUs) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardRescuedVillagers, rescuedVillagerCount)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SaveUs = SaveUs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

saveUs :: TreacheryCard SaveUs
saveUs = treachery SaveUs Cards.saveUs

-- | When Revealed: Each player must choose: either discard 2 villager tokens from Rescued
-- Villagers, or search the encounter deck and discard pile for an enemy, reveal it, and add
-- it to the staging area. Shuffle the encounter deck. With no villager left to discard, a
-- player searches.
-- Shadow: Attacking enemy gets +1 attack. Deal it another shadow card.
instance RunMessage SaveUs where
  runMessage msg t@(SaveUs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        rescued <- rescuedVillagerCount
        if rescued > 0
          then chooseOneM pid do
            questionLabeled "saveUs.choose"
            causedBy attrs
            labeled "saveUs.discardVillagers" $ discardRescuedVillagers attrs 2
            labeled "saveUs.search" $ search pid
          else search pid
      shuffleEncounterDeck
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      dealShadowCardsDuringAttack eid pid defenders 1
      pure t
    _ -> SaveUs <$> runMessage msg attrs
   where
    isEnemy = (== EnemyType) . cdCardType . toCardDef
    search :: ReverseQueue m => PlayerId -> m ()
    search pid = do
      deck <- getsGame (filter isEnemy . (.encounterDeck) . toAttrs . (.scenario))
      discarded <- filter isEnemy <$> getEncounterDiscard
      chooseOrRunOneM pid do
        questionLabeled "saveUs.enemy"
        causedBy attrs
        targets deck revealFromEncounterDeck
        targets discarded revealFromEncounterDiscard
