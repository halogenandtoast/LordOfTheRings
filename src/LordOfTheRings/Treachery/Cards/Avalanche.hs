module LordOfTheRings.Treachery.Cards.Avalanche (avalanche) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Avalanche = Avalanche TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

avalanche :: TreacheryCard Avalanche
avalanche = treachery Avalanche Cards.avalanche

-- | When Revealed: Exhaust each ready character and if it is the quest phase commit them
-- to the quest. (One that cannot commit, as Freezing Cold's host, is only exhausted.)
instance RunMessage Avalanche where
  runMessage msg t@(Avalanche attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questPhase <- getsGame ((== QuestPhase) . (.phase))
      ready' <- selectCharacters ReadyHero ReadyAlly
      for_ ready' \c -> do
        exhaust c
        barred <- elem CannotCommitToQuest <$> getModifiers c
        when (questPhase && not barred) $ commitToQuest c
      pure t
    _ -> Avalanche <$> runMessage msg attrs
