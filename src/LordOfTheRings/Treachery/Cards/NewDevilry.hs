module LordOfTheRings.Treachery.Cards.NewDevilry (newDevilry) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers (namelessFearThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NewDevilry = NewDevilry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

newDevilry :: TreacheryCard NewDevilry
newDevilry = treachery NewDevilry Cards.newDevilry

-- | When Revealed: If the players are not on stage 1, shuffle the current quest card into
-- the quest deck, then reveal a new quest card. Otherwise, New Devilry gains surge.
-- Shadow: If this attack is undefended, raise your threat by The Nameless Fear's threat.
instance RunMessage NewDevilry where
  runMessage msg t@(NewDevilry attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      mains <- getMainQuests
      quest <- getsGame (find ((`elem` mains) . toTarget) . toList . (.entities.quests))
      case quest of
        Just q | q.stage /= 1 -> do
          shuffleIntoQuestDeck q
          advanceQuest
        _ -> revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid [] | cid == attrs.cardId -> do
      encounterRaisesThreat pid =<< namelessFearThreat
      pure t
    _ -> NewDevilry <$> runMessage msg attrs
