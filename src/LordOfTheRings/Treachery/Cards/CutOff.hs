module LordOfTheRings.Treachery.Cards.CutOff (cutOff) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CutOff = CutOff TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cutOff :: TreacheryCard CutOff
cutOff = treachery CutOff Cards.cutOff

instance RunMessage CutOff where
  runMessage msg t@(CutOff a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      chooseOneM you do
        questionLabeled "cutOff.choose"
        causedBy a
        labeled "cutOff.damage" do
          characters <- charactersControlledBy you
          for_ characters (\c -> dealDamage a c 1)
        labeled "cutOff.noProgress" $ roundModifier a GameTarget NoProgressOnQuestCards
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      current <- getCurrentQuest
      quests <- filter (\q -> q.progress > 0 && Just (toTarget q) == current) . toList <$> getsGame (.entities.quests)
      for_ quests (\q -> removeTokens a q Progress q.progress)
      pure t
    _ -> CutOff <$> runMessage msg a
