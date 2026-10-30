module LordOfTheRings.Treachery.Cards.CaveIn (caveIn) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaveIn = CaveIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveIn :: TreacheryCard CaveIn
caveIn = treachery CaveIn Cards.caveIn

-- | When Revealed: Remove all progress tokens from the current quest card and active
-- location. If Cave In removed no progress tokens, it gains Surge.
instance RunMessage CaveIn where
  runMessage msg t@(CaveIn attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      current <- getCurrentQuest
      quests <- filter (\q -> q.progress > 0 && Just (toTarget q) == current) . toList <$> getsGame (.entities.quests)
      active <- filter ((> 0) . (.progress)) <$> (traverse getLocation =<< select IsActiveLocation)
      -- an active location immune to card effects keeps its progress
      removed <- filterM (fmap not . immuneToSource (toSource attrs)) active
      for_ quests \q -> removeTokens attrs q Progress q.progress
      for_ active \l -> removeTokens attrs l Progress l.progress
      when (null quests && null removed) revealEncounterCard
      pure t
    _ -> CaveIn <$> runMessage msg attrs
